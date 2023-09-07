library(tidyverse)
library(openxlsx)
library(priceR)
library(xts)
library(zoo)
library(lubridate)
library(eply)
library(readxl)
library(Hmisc)
library(conflicted)
library(quantmod)

# Prefer dplyr over other packages for a handful of functions
conflicts_prefer(dplyr::filter(), dplyer::mutate(), base::as.numeric(), dplyr::summarize())

# Function to normalize values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Read in Consumer Expenditure (CES) and Retail Trade Survey Data (ARTS)
CES <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 1)
ARTS <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 2)

# Filter both datasets for 1994-current
CES <- CES %>%
  filter(Year >= 1994)
ARTS <- ARTS %>%
  filter(Year >= 1994)

# Read in the Inflation Adjustment Spreadsheet
Inflation <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/InflationRate.xlsx")
# Select the year and rate columns
Inflation <- Inflation[,c(1,3)]

# Convert 'Year' to numeric, and join the inflation dataset to the ARTS data by year
ARTS <- ARTS %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(Inflation, by = 'Year')
# Inflation adjust all numeric columns of ARTS
ARTS[2:21] <- ARTS[2:21] * ARTS$Rate
# Drop the inflation rate variable
ARTS <- ARTS %>%
  select(-Rate)

# Join the inflation dataset to the CES data by year
CES <- CES %>%
  left_join(Inflation, by = 'Year')
# Inflation adjust all numeric columns of CES
CES[2:29] <- CES[2:29] * CES$Rate
# Drop the inflation rate variable
CES <- CES %>%
  select(-Rate)

# Remove 'Inflation'
rm(Inflation)

# Read in the AHS Panel and filter for 1997-current (issues with 1995 data)
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx") %>%
  filter(Year >= 1997)

# Create seperate dataframes for each of the cost classes as well as DisRepair jobs
Mini <- Data %>%
  filter(CostClass == 'Mini')
Small <- Data %>%
  filter(CostClass == 'Small')
Medium <- Data %>%
  filter(CostClass == 'Medium')
Large <- Data %>%
  filter(CostClass == 'Large')
DisRepair <- Data %>%
  filter(CostClass == 'DisRepairs')
# Maint <- Data %>%
#   filter(CostClass == 'Maintenance')


# Define a 'Years' variable
Years <- c(seq(1997, 2021, 2))

# Define 'CESVars' to contain all variable names from the CES data (not including 'Year' of course)
CESVars <- names(CES[, 2:29])
# Establish an empty list, this will be populated below
CESList <- list()

# For each CES Variable and Year combination
for (x in 1:length(CESVars)) {
  for (i in Years) {
    # Append to 'CESList' the following: (Current Year + Prior Year)/2 
    CESList <- append(CESList, 
                      (CES[CES$Year == i, ][, CESVars[x]] + CES[CES$Year == i-1, ][, CESVars[x]]) / 2)
                  
  }
}

# Convert CESList to a dataframe
CESList <- as.data.frame(CESList)
#Pivot CESList to a long format
CESList <- CESList %>%
  pivot_longer(cols = 1:364)
# Store the raw CES data as 'CESRaw'
CESRaw <- CES
# Create a template dataframe to place the average 2-year CES values in, nrow = 13 for 13 AHS years (1997-2021), ncol = 28 for 28 CES variables
CES <- data.frame(matrix(NA, nrow = 13, ncol = 28))
# Transer variable names to CES from CESVars
colnames(CES) <- CESVars

# For each number 1-28 (28 = number of CES variables)
for (i in 1:28) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 13 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 13
  # Paste the values within start_row/end_row into the ith column in CES...This will place each variable's value into a seperate column.
  CES[, i] <- CESList[start_row:end_row, 2, drop = FALSE]
}

# Create a 'Years' column
CES$Year <- Years
# Reorder CES so 'Years' is the first column
CES <- CES[, c(29, 1:28)]
# Convert 'Year' to a date object
CES$Year <- as.Date(paste0(CES$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert CES to an xts object
CES <- as.xts(CES)

# Calculate the growth rate for each variable, store as CESDiff
CESDiff <- diff(CES) / dplyr::lag(CES, n = 1)

# Store the names of ARTS variables under ARTSVars
ARTSVars <- names(ARTS[, 2:21])
# Establish a few empty lists, these will be populated below
ARTSList <- list()

# For each AHS year
for (x in 1:length(ARTSVars)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    ARTSList <- append(ARTSList, 
                      (ARTS[ARTS$Year == i, ][, ARTSVars[x]] + ARTS[ARTS$Year == i-1, ][, ARTSVars[x]]))
  }
}

# Convert ARTSList to a dataframe
ARTSList <- as.data.frame(ARTSList)
# Pivot ARTSList to a long format
ARTSList <- ARTSList %>%
  pivot_longer(cols = 1:260)

# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
ARTS <- data.frame(matrix(NA, nrow = 13, ncol = 20))
# Transfer variable names to column names for ARTS 2-year data
colnames(ARTS) <- ARTSVars

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:20) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 13 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 13
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  ARTS[, i] <- ARTSList[start_row:end_row, 2, drop = FALSE]
}

# Create a 'Years' column
ARTS$Year <- Years
# Reorder columns so year is first
ARTS <- ARTS[, c(21, 1:20)]
# Convert the Year variable from numeric to a date
ARTS$Year <- as.Date(paste0(ARTS$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
# Create an xts (timeseries) dataset using ARTS
ARTS <- as.xts(ARTS)

# Calculate the growth rate manually using diff
ARTSDiff <- diff(ARTS) / dplyr::lag(ARTS, n = 1)


# Create 'AllData' containing the TotalJobCost for each of the Cost Classes + DisRepairs/Maintanence
AllData <- data.frame(Year = Mini$Year,
                      Mini = Mini$TotalJobCost,
                      Small = Small$TotalJobCost,
                      Medium = Medium$TotalJobCost,
                      Large = Large$TotalJobCost,
                      DisRepair = DisRepair$TotalJobCost)
                      #Maint = Maint$TotalJobCost)

# Create a 'Years' column as a date
AllData$Year <- as.Date(paste0(AllData$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
AllData <- as.xts(AllData)
# Create AllDataDiff, this will contain 2-year growth rates.
AllDataDiff <- diff(AllData) / dplyr::lag(AllData, n = 1)

# Bind the growth rate data to the original Data by column
AllData <- AllData %>%
  cbind(AllDataDiff)

# Convert AllData to a dataframe
AllData <- as.data.frame(AllData)
# Create a Years column
AllData$Year <- Years

CESDiff <- as.data.frame(CESDiff)
CESDiff$Year <- Years
CESDiff <- CESDiff[, c(29, 1:28)]

ARTSDiff <- as.data.frame(ARTSDiff)
ARTSDiff$Year <- Years
ARTSDiff <- ARTSDiff[, c(21, 1:20)]


ARTSDiff <- ARTSDiff %>%
  filter(Year > 1997)
CESDiff <- CESDiff %>%
  filter(Year > 1997)
AllData <- AllData %>%
  filter(Year > 1997)
AllData <- AllData[, c(11, 1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]
AllData <- AllData %>%
  rename(MiniDiff = Mini.1, SmallDiff = Small.1, MediumDiff = Medium.1, LargeDiff = Large.1, DisRepairDiff = DisRepair.1)


# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
ARTSMiniCor <- as.data.frame(cor(AllData$MiniDiff, ARTSDiff[, c(2:21)]))
ARTSMiniCor <- ARTSMiniCor %>%
  pivot_longer(cols = c(1:20))

ARTSSmallCor <- as.data.frame(cor(AllData$SmallDiff, ARTSDiff[, c(2:21)]))
ARTSSmallCor <- ARTSSmallCor %>%
  pivot_longer(cols = c(1:20))

ARTSMediumCor <- as.data.frame(cor(AllData$MediumDiff, ARTSDiff[, c(2:21)]))
ARTSMediumCor <- ARTSMediumCor %>%
  pivot_longer(cols = c(1:20))

ARTSLargeCor <- as.data.frame(cor(AllData$LargeDiff, ARTSDiff[, c(2:21)]))
ARTSLargeCor <- ARTSLargeCor %>%
  pivot_longer(cols = c(1:20))

ARTSDisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, ARTSDiff[, c(2:21)]))
ARTSDisRepairCor <- ARTSDisRepairCor %>%
  pivot_longer(cols = c(1:20))

# ARTSMaintCor <- as.data.frame(cor(AllData$MaintDiff, ARTSDiff[, c(2:21)]))
# ARTSMaintCor <- ARTSMaintCor %>%
#   pivot_longer(cols = c(1:20))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
ARTS_Correlations <- data.frame(Variable = ARTSMiniCor$name,
                           Mini = ARTSMiniCor$value,
                           Small = ARTSSmallCor$value,
                           Medium = ARTSMediumCor$value,
                           Large = ARTSLargeCor$value,
                           DisRepair = ARTSDisRepairCor$value)
                           #Maint = ARTSMaintCor$value)


# Create CES[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in CES, convert to a dataframe, and pivot the dataframe to a long format
CESMiniCor <- as.data.frame(cor(AllData$MiniDiff, CESDiff[, c(2:29)]))
CESMiniCor <- CESMiniCor %>%
  pivot_longer(cols = c(1:28))

CESSmallCor <- as.data.frame(cor(AllData$SmallDiff, CESDiff[, c(2:29)]))
CESSmallCor <- CESSmallCor %>%
  pivot_longer(cols = c(1:28))

CESMediumCor <- as.data.frame(cor(AllData$MediumDiff, CESDiff[, c(2:29)]))
CESMediumCor <- CESMediumCor %>%
  pivot_longer(cols = c(1:28))

CESLargeCor <- as.data.frame(cor(AllData$LargeDiff, CESDiff[, c(2:29)]))
CESLargeCor <- CESLargeCor %>%
  pivot_longer(cols = c(1:28))

CESDisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, CESDiff[, c(2:29)]))
CESDisRepairCor <- CESDisRepairCor %>%
  pivot_longer(cols = c(1:28))

# CESMaintCor <- as.data.frame(cor(AllData$MaintDiff, CESDiff[, c(2:29)]))
# CESMaintCor <- CESMaintCor %>%
#   pivot_longer(cols = c(1:28))

# Condense all correlations into one dataframe called 'CES_Correlations'
CES_Correlations <- data.frame(Variable = CESMiniCor$name,
                           Mini = CESMiniCor$value,
                           Small = CESSmallCor$value,
                           Medium = CESMediumCor$value,
                           Large = CESLargeCor$value,
                           DisRepair = CESDisRepairCor$value)
                           #Maint = CESMaintCor$value)

# Remove unnessecary objects
rm(ARTSDisRepairCor, ARTSLargeCor, ARTSMaintCor, ARTSMediumCor, ARTSMiniCor, ARTSSmallCor, CESDisRepairCor, CESLargeCor, CESMaintCor, CESMediumCor, CESMiniCor, CESSmallCor)

# The next portion finds P-Vals for each of the correlation values and appends them to the respective '[CES/ARTS]_Correlations' dataframe

# Create a few empty lists, these will be populated in the following for-in loop
MiniPVal <- list()
SmallPVal <- list()
MediumPVal <- list()
LargePVal <- list()
DisRepairPVal <- list()
MaintPVal <- list()

# For each variable in the ARTS data
for (i in names(ARTSDiff[,2:21])) {
  # Append the respective list with the output from rcorr(), testing the normalized TotalJobCost against the normalized value from ARTS
  MiniPVal <- append(MiniPVal, rcorr(AllData$MiniDiff, ARTSDiff[,i]))
  SmallPVal <- append(SmallPVal, rcorr(AllData$SmallDiff, ARTSDiff[,i]))
  MediumPVal <- append(MediumPVal, rcorr(AllData$MediumDiff, ARTSDiff[,i]))
  LargePVal <- append(LargePVal, rcorr(AllData$LargeDiff, ARTSDiff[,i]))
  DisRepairPVal <- append(DisRepairPVal, rcorr(AllData$DisRepairDiff, ARTSDiff[,i]))
  #MaintPVal <- append(MaintPVal, rcorr(AllData$MaintDiff, ARTSDiff[,i]))
  
}

# At this point all of the lists established above are not in a readable format

# First convert the list to a dataframe
MiniPVal <- as.data.frame(MiniPVal)
# Select variables that start with 'P.x', these are all of the p-values for the correlation between ARTS variables and the TotalJobCost in question (Mini here)
MiniPVal <- MiniPVal %>%
  select(starts_with(c("P.x")))
# Drop the first row, it is all NA
MiniPVal<- MiniPVal[-1,]
# Conver column names to the appropriate variable name from the ARTS data
colnames(MiniPVal) <- names(ARTSDiff[,2:21])
# Pivot the dataframe to a long format
MiniPVal <- MiniPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MiniPVal')

# All following chunks of code (lines 192-230) follow the same pipeline as above

SmallPVal <- as.data.frame(SmallPVal)
SmallPVal <- SmallPVal %>%
  select(starts_with(c("P.x")))
SmallPVal<- SmallPVal[-1,]
colnames(SmallPVal) <- names(ARTSDiff[,2:21])
SmallPVal <- SmallPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'SmallPVal')

MediumPVal <- as.data.frame(MediumPVal)
MediumPVal <- MediumPVal %>%
  select(starts_with(c("P.x")))
MediumPVal<- MediumPVal[-1,]
colnames(MediumPVal) <- names(ARTSDiff[,2:21])
MediumPVal <- MediumPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MediumPVal')

LargePVal <- as.data.frame(LargePVal)
LargePVal <- LargePVal %>%
  select(starts_with(c("P.x")))
LargePVal<- LargePVal[-1,]
colnames(LargePVal) <- names(ARTSDiff[,2:21])
LargePVal <- LargePVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'LargePVal')

DisRepairPVal <- as.data.frame(DisRepairPVal)
DisRepairPVal <- DisRepairPVal %>%
  select(starts_with(c("P.x")))
DisRepairPVal<- DisRepairPVal[-1,]
colnames(DisRepairPVal) <- names(ARTSDiff[,2:21])
DisRepairPVal <- DisRepairPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'DisRepairPVal')

# MaintPVal <- as.data.frame(MaintPVal)
# MaintPVal <- MaintPVal %>%
#   select(starts_with(c("P.x")))
# MaintPVal<- MaintPVal[-1,]
# colnames(MaintPVal) <- names(ARTSDiff[,2:21])
# MaintPVal <- MaintPVal %>%
#   pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MaintPVal')

# Create a dataframe of all pvals
PVals <- data.frame('MiniPVal' = MiniPVal$MiniPVal,
                    'SmallPVal' = SmallPVal$SmallPVal,
                    'MediumPVal' = MediumPVal$MediumPVal,
                    'LargePVal' = LargePVal$LargePVal,
                    'DisRepairPVal' = DisRepairPVal$DisRepairPVal)
                    #'MaintPVal' = MaintPVal$MaintPVal)

# Bind the pvals dataframe to ARTS_Correlations
ARTS_Correlations <- ARTS_Correlations %>%
  cbind(PVals)

# Reorder ARTS_Correlations so that each p-value column is directly after the correlation value it corresponds to
ARTS_Correlations <- ARTS_Correlations[, c(1:2, 7, 3, 8, 4, 9, 5, 10, 6, 11)]

# Remove all intermediate items, these variables will be used again below
rm(Mini, MiniPVal, Small, SmallPVal, Medium, MediumPVal, Large, LargePVal, DisRepair, DisRepairPVal, Maint, MaintPVal, PVals)

# Create a few empty lists, these will be populated in the following for-in loop
MiniPVal <- list()
SmallPVal <- list()
MediumPVal <- list()
LargePVal <- list()
DisRepairPVal <- list()
#MaintPVal <- list()

# For each variable in the CES data
for (i in names(CESDiff[,2:29])) {
  # Append the respective list with the output from rcorr(), testing the normalized TotalJobCost against the normalized value from CES
  MiniPVal <- append(MiniPVal, rcorr(AllData$MiniDiff, CESDiff[,i]))
  SmallPVal <- append(SmallPVal, rcorr(AllData$SmallDiff, CESDiff[,i]))
  MediumPVal <- append(MediumPVal, rcorr(AllData$MediumDiff, CESDiff[,i]))
  LargePVal <- append(LargePVal, rcorr(AllData$LargeDiff, CESDiff[,i]))
  DisRepairPVal <- append(DisRepairPVal, rcorr(AllData$DisRepairDiff, CESDiff[,i]))
  #MaintPVal <- append(MaintPVal, rcorr(AllData$MaintDiff, CESDiff[,i]))
  
}

# First convert the list to a dataframe
MiniPVal <- as.data.frame(MiniPVal)
# Select variables that start with 'P.x', these are all of the p-values for the correlation between ARTS variables and the TotalJobCost in question (Mini here)
MiniPVal <- MiniPVal %>%
  select(starts_with(c("P.x")))
# Drop the first row, it is all NA
MiniPVal<- MiniPVal[-1,]
# Conver column names to the appropriate variable name from the ARTS data
colnames(MiniPVal) <- names(CESDiff[,2:29])
# Pivot the dataframe to a long format
MiniPVal <- MiniPVal %>%
  pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'MiniPVal')

# All following chunks of code (lines 285-323) follow the same pipeline as above

SmallPVal <- as.data.frame(SmallPVal)
SmallPVal <- SmallPVal %>%
  select(starts_with(c("P.x")))
SmallPVal<- SmallPVal[-1,]
colnames(SmallPVal) <- names(CESDiff[,2:29])
SmallPVal <- SmallPVal %>%
  pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'SmallPVal')

MediumPVal <- as.data.frame(MediumPVal)
MediumPVal <- MediumPVal %>%
  select(starts_with(c("P.x")))
MediumPVal<- MediumPVal[-1,]
colnames(MediumPVal) <- names(CESDiff[,2:29])
MediumPVal <- MediumPVal %>%
  pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'MediumPVal')

LargePVal <- as.data.frame(LargePVal)
LargePVal <- LargePVal %>%
  select(starts_with(c("P.x")))
LargePVal<- LargePVal[-1,]
colnames(LargePVal) <- names(CESDiff[,2:29])
LargePVal <- LargePVal %>%
  pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'LargePVal')

DisRepairPVal <- as.data.frame(DisRepairPVal)
DisRepairPVal <- DisRepairPVal %>%
  select(starts_with(c("P.x")))
DisRepairPVal<- DisRepairPVal[-1,]
colnames(DisRepairPVal) <- names(CESDiff[,2:29])
DisRepairPVal <- DisRepairPVal %>%
  pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'DisRepairPVal')

# MaintPVal <- as.data.frame(MaintPVal)
# MaintPVal <- MaintPVal %>%
#   select(starts_with(c("P.x")))
# MaintPVal<- MaintPVal[-1,]
# colnames(MaintPVal) <- names(CESDiff[,2:29])
# MaintPVal <- MaintPVal %>%
#   pivot_longer(cols = 1:28, names_to = 'Variable', values_to = 'MaintPVal')

# Create a dataframe of all pvals
PVals <- data.frame('MiniPVal' = MiniPVal$MiniPVal,
                    'SmallPVal' = SmallPVal$SmallPVal,
                    'MediumPVal' = MediumPVal$MediumPVal,
                    'LargePVal' = LargePVal$LargePVal,
                    'DisRepairPVal' = DisRepairPVal$DisRepairPVal)
                    #'MaintPVal' = MaintPVal$MaintPVal)

# Bind the pvals dataframe to CES_Correlations
CES_Correlations <- CES_Correlations %>%
  cbind(PVals)

# Reorder CES_Correlations so that each p-value column is directly after the correlation value it corresponds to
CES_Correlations <- CES_Correlations[, c(1:2, 7, 3, 8, 4, 9, 5, 10, 6, 11)]

# Remove all intermediate items
rm(Mini, MiniPVal, Small, SmallPVal, Medium, MediumPVal, Large, LargePVal, DisRepair, DisRepairPVal, Maint, MaintPVal, PVals)

# Create a 'Years' variable corresponding to AHS Years
Years <- AllData$Year

# Read in total precipitation (US East) data
EastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation.xlsx")

# Define the years to analyze
Years <- c(seq(1997, 2021, 2))

# Store variable names in 'EastPrecipVars'
EastPrecipVars <- names(EastPrecipData[, 2:33])

# Establish an empty list, this will be populated below
EastPrecipList <- list()
# For each precipitation variable
for (x in 1:length(EastPrecipVars)) {
  # for each AHS Year
  for (i in Years) {
    # Append the sum of the total precipitation across the two years to 'EastPrecipList'
    EastPrecipList <- append(EastPrecipList, 
                           (EastPrecipData[EastPrecipData$Year == i, ][, EastPrecipVars[x]] + EastPrecipData[EastPrecipData$Year == i-1, ][, EastPrecipVars[x]]))
    
  }
}
# Convert 'EastPrecipList' to a dataframe
EastPrecipList <- as.data.frame(EastPrecipList)
# Convert 'EastPrecipList' to a long format
EastPrecipList <- EastPrecipList %>%
  pivot_longer(cols = 1:416)

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
EastPrecip <- data.frame(matrix(NA, nrow = 13, ncol = 32))
# Populate column names for the empty template with variable names (stored in 'EastPrecipVars')
colnames(EastPrecip) <- EastPrecipVars

# for each number in 1-32
for (i in 1:32) {
  # define a start row (when i = 1 this will become 1, when i = 2 this will become 14, etc.)
  start_row <- (i - 1) * 13 + 1
  # define an end row (when i = 1 this will become 13, when i = 2 this will become 26, etc.)
  end_row <- i * 13
  # Append the template dataset with the correct values for each variable
  EastPrecip[, i] <- EastPrecipList[start_row:end_row, 2, drop = FALSE]
}

# Create a year column using AHS Years
EastPrecip$Year <- Years
# Reorder columns so that year is first
EastPrecip <- EastPrecip[, c(33, 1:32)]


EastPrecip$Year <- as.Date(paste0(EastPrecip$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecip <- as.xts(EastPrecip)

# Calculate the growth rate manually using diff
EastPrecipDiff <- diff(EastPrecip) / dplyr::lag(EastPrecip, n = 1)
EastPrecipDiff <- as.data.frame(EastPrecipDiff)
EastPrecipDiff$Year <- Years
EastPrecipDiff <- EastPrecipDiff[, c(33, 1:32)]
EastPrecipDiff <- EastPrecipDiff %>%
  filter(Year > 1997)
# # Normalize all precipitation variables
# EastPrecip[, c(2:33)] <- lapply(EastPrecip[, c(2:33)], normalize)

# Create EastPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question 
EastPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, EastPrecipDiff[, c(2:33)]))

# Pivot PrecipCor to a long format
EastPrecipCor <- EastPrecipCor %>%
  pivot_longer(cols = c(1:32))

# Read in total precipitation (US Northeast) data
NortheastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USNortheast_TotalPrecipitation.xlsx")

# Store variable names in 'NEPrecipVars'
NEPrecipVars <- names(NortheastPrecipData[, 2:33])
# Establish an empty list, this will be populated below
NEPrecipList <- list()

# For each precipitation variable
for (x in 1:length(NEPrecipVars)) {
  # For each AHS Year
  for (i in Years) {
    # Append the sum of the total precipitation across the two years to 'NEPrecipList'
    NEPrecipList <- append(NEPrecipList, 
                      (NortheastPrecipData[NortheastPrecipData$Year == i, ][, NEPrecipVars[x]] + NortheastPrecipData[NortheastPrecipData$Year == i-1, ][, NEPrecipVars[x]]))
    
  }
}

# Convert 'NEPrecipList' to a dataframe
NEPrecipList <- as.data.frame(NEPrecipList)
# Convert 'NEPrecipList' to a long format
NEPrecipList <- NEPrecipList %>%
  pivot_longer(cols = 1:416)

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
NEPrecip <- data.frame(matrix(NA, nrow = 13, ncol = 32))
# Populate column names for the empty template with variable names (stored in 'NEPrecipVars')
colnames(NEPrecip) <- NEPrecipVars

# for each number in 1-32
for (i in 1:32) {
  # define a start row (when i = 1 this will become 1, when i = 2 this will become 14, etc.)
  start_row <- (i - 1) * 13 + 1
  # define an end row (when i = 1 this will become 13, when i = 2 this will become 26, etc.)
  end_row <- i * 13
  # Append the template dataset with the correct values for each variable
  NEPrecip[, i] <- NEPrecipList[start_row:end_row, 2, drop = FALSE]
}

# Create a year column using AHS Years
NEPrecip$Year <- Years
# Reorder columns so that year is first
NEPrecip <- NEPrecip[, c(33, 1:32)]

NEPrecip$Year <- as.Date(paste0(NEPrecip$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
NEPrecip <- as.xts(NEPrecip)

# Calculate the growth rate manually using diff
NEPrecipDiff <- diff(NEPrecip) / dplyr::lag(NEPrecip, n = 1)
NEPrecipDiff <- as.data.frame(NEPrecipDiff)
NEPrecipDiff$Year <- Years
NEPrecipDiff <- NEPrecipDiff[, c(33, 1:32)]
NEPrecipDiff <- NEPrecipDiff %>%
  filter(Year > 1997)
# # Normalize all precipitation variables
# NEPrecip[, c(2:33)] <- lapply(NEPrecip[, c(2:33)], normalize)

# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question 
NEPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, NEPrecipDiff[, c(2:33)]))

# Pivot NEPrecipCor to a long format
NEPrecipCor <- NEPrecipCor %>%
  pivot_longer(cols = c(1:32))

# Read in total precipitation (US Northeast) data
EastPrecip_RCP4.5 <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation_RCP4.5.xlsx")

# Store variable names in 'NEPrecipVars'
EastPrecipRCPVars <- names(EastPrecip_RCP4.5[, 2:33])
# Establish an empty list, this will be populated below
EastPrecipRCPList <- list()

# For each precipitation variable
for (x in 1:length(EastPrecipRCPVars)) {
  # For each AHS Year
  for (i in Years) {
    # Append the sum of the total precipitation across the two years to 'NEPrecipList'
    EastPrecipRCPList <- append(EastPrecipRCPList, 
                           (EastPrecip_RCP4.5[EastPrecip_RCP4.5$Year == i, ][, EastPrecipRCPVars[x]] + EastPrecip_RCP4.5[EastPrecip_RCP4.5$Year == i-1, ][, EastPrecipRCPVars[x]]))
    
  }
}

# Convert 'NEPrecipList' to a dataframe
EastPrecipRCPList <- as.data.frame(EastPrecipRCPList)
# Convert 'NEPrecipList' to a long format
EastPrecipRCPList <- EastPrecipRCPList %>%
  pivot_longer(cols = 1:416)

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
EastPrecipRCP <- data.frame(matrix(NA, nrow = 13, ncol = 32))
# Populate column names for the empty template with variable names (stored in 'NEPrecipVars')
colnames(EastPrecipRCP) <- EastPrecipRCPVars

# for each number in 1-32
for (i in 1:32) {
  # define a start row (when i = 1 this will become 1, when i = 2 this will become 14, etc.)
  start_row <- (i - 1) * 13 + 1
  # define an end row (when i = 1 this will become 13, when i = 2 this will become 26, etc.)
  end_row <- i * 13
  # Append the template dataset with the correct values for each variable
  EastPrecipRCP[, i] <- EastPrecipRCPList[start_row:end_row, 2, drop = FALSE]
}

# Create a year column using AHS Years
EastPrecipRCP$Year <- Years
# Reorder columns so that year is first
EastPrecipRCP <- EastPrecipRCP[, c(33, 1:32)]



EastPrecipRCP$Year <- as.Date(paste0(EastPrecipRCP$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecipRCP <- as.xts(EastPrecipRCP)

# Calculate the growth rate manually using diff
EastPrecipRCPDiff <- diff(EastPrecipRCP) / dplyr::lag(EastPrecipRCP, n = 1)
EastPrecipRCPDiff <- as.data.frame(EastPrecipRCPDiff)
EastPrecipRCPDiff$Year <- Years
EastPrecipRCPDiff <- EastPrecipRCPDiff[, c(33, 1:32)]
EastPrecipRCPDiff <- EastPrecipRCPDiff %>%
  filter(Year > 1997)
# # Normalize all precipitation variables
# EastPrecipRCP[, c(2:33)] <- lapply(EastPrecipRCP[, c(2:33)], normalize)

# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question 
EastPrecipRCPCor <- as.data.frame(cor(AllData$DisRepairDiff, EastPrecipRCPDiff[, c(2:33)]))

# Pivot NEPrecipCor to a long format
EastPrecipRCPCor <- EastPrecipRCPCor %>%
  pivot_longer(cols = c(1:32))

# Create a 'Source' column for each correlation dataframe
ARTS_Correlations$Source <- 'ARTS'
CES_Correlations$Source <- 'CES'
EastPrecipCor$Source <- 'CR_EastUS'
NEPrecipCor$Source <- 'CR_NEUS'
EastPrecipRCPCor$Source <- 'CR_EastUS_RCP4.5'

# Bind the ARTS and CES correlation data together
AllCorrelations <- ARTS_Correlations %>%
  rbind(CES_Correlations)

# Bind the precipitation correlation data together and rename columns to allow for joining to AllCorrelations
PrecipCorrelations <- EastPrecipCor %>%
  rbind(NEPrecipCor, EastPrecipRCPCor) %>%
  rename(DisRepair = value, Variable = name)

# Bind the precipitation forrelation data with the ARTS/CES correlation data
AllCorrelations <- AllCorrelations %>%
  plyr::rbind.fill(PrecipCorrelations)

# Amend the variable label for precipitation data to include the source (i.e. '_EastUS')
AllCorrelations <- AllCorrelations %>%
  mutate(Variable = ifelse(Source == 'CR_EastUS', paste0(Variable, '_EastUS'), Variable),
         Variable = ifelse(Source == 'CR_NEUS', paste0(Variable, '_NortheastUS'), Variable),
         Variable = ifelse(Source == 'CR_EastUS_RCP4.5', paste0(Variable, '_EastUS_RCP4.5'), Variable))

# Read in the Variable description spreadsheet
VariableDescriptions <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 5)

# Join the variable descriptions to the correlations dataframe
AllCorrelations <- AllCorrelations %>%
  left_join(VariableDescriptions[,c(1:2)], by = 'Variable')

# Reorder the Correlations dataframe
AllCorrelations <- AllCorrelations[,c(1,13,12,2:11)]

# Find the sd for both of the DisRepair predictors
SD_NEApr <- sd(NEPrecipDiff$Apr)
SD_FuelDealers <- sd(ARTSDiff$FuelDealers)

# Output the correlations data
write.xlsx(AllCorrelations, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_9.6.xlsx")

ggplot(AllData, aes(Year, DisRepairDiff)) +
  geom_line(color = 'red') +
  labs(title = 'White = DisReapir Project Spending') +
  geom_line(data = EastPrecipDiff, aes(Year, Apr), color = 'yellow') +
  geom_line(data = ARTSDiff, aes(Year, FuelDealers), color = 'green') +
  scale_x_continuous(breaks = c(seq(1997,2021,2)))+
  theme_jbrec()


