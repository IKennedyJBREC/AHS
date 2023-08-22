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

conflicts_prefer(dplyr::filter(), dplyer::mutate(), base::as.numeric(), dplyr::summarize())

# Function to normalize values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Read in Consumer Expenditure (CES) and Retail Trade Survey Data (ARTS)
CES <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 1)
ARTS <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 2)

# Filter both datasets for 1997-current
CES <- CES %>%
  filter(Year >= 1996)
ARTS <- ARTS %>%
  filter(Year >= 1996)

# Read in the AHS Panel and filter for 1997-current (issues with 1995 data)
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long.xlsx") %>%
  filter(Year >= 1997)


# Create seperate dataframes for each of the cost classes as well as DisRepairs and Maintanence jobs
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
Maint <- Data %>%
  filter(CostClass == 'Maintenance')

Years <- c(seq(1997, 2021, 2))
CESVars <- names(CES[, 2:11])
# Establish a few empty lists, these will be populated below
CESList <- list()
# For each AHS year
for (x in 1:length(CESVars)) {
  for (i in Years) {
    CESList <- append(CESList, 
                      (CES[CES$Year == i, ][, CESVars[x]] + CES[CES$Year == i-1, ][, CESVars[x]]) / 2)
                      
  }
}

CESList <- as.data.frame(CESList)
CESList <- CESList %>%
  pivot_longer(cols = 1:130)

CES <- data.frame(matrix(NA, nrow = 13, ncol = 10))
colnames(CES) <- CESVars

for (i in 1:10) {
  start_row <- (i - 1) * 13 + 1
  end_row <- i * 13
  CES[, i] <- CESList[start_row:end_row, 2, drop = FALSE]
}

CES$Year <- Years
CES <- CES[, c(11, 1:10)]



ARTSVars <- names(ARTS[, 2:21])
# Establish a few empty lists, these will be populated below
ARTSList <- list()
# For each AHS year
for (x in 1:length(ARTSVars)) {
  for (i in Years) {
    ARTSList <- append(ARTSList, 
                      (ARTS[ARTS$Year == i, ][, ARTSVars[x]] + ARTS[ARTS$Year == i-1, ][, ARTSVars[x]]) / 2)
    
  }
}

ARTSList <- as.data.frame(ARTSList)
ARTSList <- ARTSList %>%
  pivot_longer(cols = 1:260)

ARTS <- data.frame(matrix(NA, nrow = 13, ncol = 20))
colnames(ARTS) <- ARTSVars

for (i in 1:20) {
  start_row <- (i - 1) * 13 + 1
  end_row <- i * 13
  ARTS[, i] <- ARTSList[start_row:end_row, 2, drop = FALSE]
}

ARTS$Year <- Years
ARTS <- ARTS[, c(21, 1:20)]
ARTS1 <- ARTS




# Normalize the CES and ARTS data
CES[, c(2:11)] <- lapply(CES[, c(2:11)], normalize)
ARTS[, c(2:21)] <- lapply(ARTS[, c(2:21)], normalize)

# Create 'AllData' containing the TotalJobCost for each of the Cost Classes + DisRepairs/Maintanence
AllData <- data.frame(Year = Mini$Year,
                      Mini = Mini$TotalJobCost,
                      Small = Small$TotalJobCost,
                      Medium = Medium$TotalJobCost,
                      Large = Large$TotalJobCost,
                      DisRepair = DisRepair$TotalJobCost,
                      Maint = Maint$TotalJobCost)

# Create 'Normalized' containing the normalized TotalJobCosts
Normalized <- lapply(AllData[, c(2:7)], normalize)
# Convert Normalized to a dataframe
Normalized <- as.data.frame(Normalized)
# Append '_Normalized' to each of the column names
names(Normalized) <- paste0(names(Normalized), '_Normalized')

# Bind the normalized data to the raw data
AllData <- AllData %>%
  cbind(Normalized)

# Further filter ARTS for odd-year data
ARTS <- ARTS %>%
  filter(Year %in% AllData$Year)

# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
ARTSMiniCor <- as.data.frame(cor(AllData$Mini_Normalized, ARTS[, c(2:21)]))
ARTSMiniCor <- ARTSMiniCor %>%
  pivot_longer(cols = c(1:20))

ARTSSmallCor <- as.data.frame(cor(AllData$Small_Normalized, ARTS[, c(2:21)]))
ARTSSmallCor <- ARTSSmallCor %>%
  pivot_longer(cols = c(1:20))

ARTSMediumCor <- as.data.frame(cor(AllData$Medium_Normalized, ARTS[, c(2:21)]))
ARTSMediumCor <- ARTSMediumCor %>%
  pivot_longer(cols = c(1:20))

ARTSLargeCor <- as.data.frame(cor(AllData$Large_Normalized, ARTS[, c(2:21)]))
ARTSLargeCor <- ARTSLargeCor %>%
  pivot_longer(cols = c(1:20))

ARTSDisRepairCor <- as.data.frame(cor(AllData$DisRepair_Normalized, ARTS[, c(2:21)]))
ARTSDisRepairCor <- ARTSDisRepairCor %>%
  pivot_longer(cols = c(1:20))

ARTSMaintCor <- as.data.frame(cor(AllData$Maint_Normalized, ARTS[, c(2:21)]))
ARTSMaintCor <- ARTSMaintCor %>%
  pivot_longer(cols = c(1:20))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
ARTS_Correlations <- data.frame(Variable = ARTSMaintCor$name,
                           Mini = ARTSMiniCor$value,
                           Small = ARTSSmallCor$value,
                           Medium = ARTSMediumCor$value,
                           Large = ARTSLargeCor$value,
                           DisRepair = ARTSDisRepairCor$value,
                           Maint = ARTSMaintCor$value)

# Further filter CES for odd-year data
CES <- CES %>%
  filter(Year %in% Mini$Year)


# Create CES[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in CES, convert to a dataframe, and pivot the dataframe to a long format
CESMiniCor <- as.data.frame(cor(AllData$Mini_Normalized, CES[, c(2:11)]))
CESMiniCor <- CESMiniCor %>%
  pivot_longer(cols = c(1:10))

CESSmallCor <- as.data.frame(cor(AllData$Small_Normalized, CES[, c(2:11)]))
CESSmallCor <- CESSmallCor %>%
  pivot_longer(cols = c(1:10))

CESMediumCor <- as.data.frame(cor(AllData$Medium_Normalized, CES[, c(2:11)]))
CESMediumCor <- CESMediumCor %>%
  pivot_longer(cols = c(1:10))

CESLargeCor <- as.data.frame(cor(AllData$Large_Normalized, CES[, c(2:11)]))
CESLargeCor <- CESLargeCor %>%
  pivot_longer(cols = c(1:10))

CESDisRepairCor <- as.data.frame(cor(AllData$DisRepair_Normalized, CES[, c(2:11)]))
CESDisRepairCor <- CESDisRepairCor %>%
  pivot_longer(cols = c(1:10))

CESMaintCor <- as.data.frame(cor(AllData$Maint_Normalized, CES[, c(2:11)]))
CESMaintCor <- CESMaintCor %>%
  pivot_longer(cols = c(1:10))

# Condense all correlations into one dataframe called 'CES_Correlations'
CES_Correlations <- data.frame(Variable = CESMaintCor$name,
                           Mini = CESMiniCor$value,
                           Small = CESSmallCor$value,
                           Medium = CESMediumCor$value,
                           Large = CESLargeCor$value,
                           DisRepair = CESDisRepairCor$value,
                           Maint = CESMaintCor$value)

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
for (i in names(ARTS[,2:21])) {
  # Append the respective list with the output from rcorr(), testing the normalized TotalJobCost against the normalized value from ARTS
  MiniPVal <- append(MiniPVal, rcorr(AllData$Mini_Normalized, ARTS[,i]))
  SmallPVal <- append(SmallPVal, rcorr(AllData$Small_Normalized, ARTS[,i]))
  MediumPVal <- append(MediumPVal, rcorr(AllData$Medium_Normalized, ARTS[,i]))
  LargePVal <- append(LargePVal, rcorr(AllData$Large_Normalized, ARTS[,i]))
  DisRepairPVal <- append(DisRepairPVal, rcorr(AllData$DisRepair_Normalized, ARTS[,i]))
  MaintPVal <- append(MaintPVal, rcorr(AllData$Maint_Normalized, ARTS[,i]))
  
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
colnames(MiniPVal) <- names(ARTS[,2:21])
# Pivot the dataframe to a long format
MiniPVal <- MiniPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MiniPVal')

# All following chunks of code (lines 192-230) follow the same pipeline as above

SmallPVal <- as.data.frame(SmallPVal)
SmallPVal <- SmallPVal %>%
  select(starts_with(c("P.x")))
SmallPVal<- SmallPVal[-1,]
colnames(SmallPVal) <- names(ARTS[,2:21])
SmallPVal <- SmallPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'SmallPVal')

MediumPVal <- as.data.frame(MediumPVal)
MediumPVal <- MediumPVal %>%
  select(starts_with(c("P.x")))
MediumPVal<- MediumPVal[-1,]
colnames(MediumPVal) <- names(ARTS[,2:21])
MediumPVal <- MediumPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MediumPVal')

LargePVal <- as.data.frame(LargePVal)
LargePVal <- LargePVal %>%
  select(starts_with(c("P.x")))
LargePVal<- LargePVal[-1,]
colnames(LargePVal) <- names(ARTS[,2:21])
LargePVal <- LargePVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'LargePVal')

DisRepairPVal <- as.data.frame(DisRepairPVal)
DisRepairPVal <- DisRepairPVal %>%
  select(starts_with(c("P.x")))
DisRepairPVal<- DisRepairPVal[-1,]
colnames(DisRepairPVal) <- names(ARTS[,2:21])
DisRepairPVal <- DisRepairPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'DisRepairPVal')

MaintPVal <- as.data.frame(MaintPVal)
MaintPVal <- MaintPVal %>%
  select(starts_with(c("P.x")))
MaintPVal<- MaintPVal[-1,]
colnames(MaintPVal) <- names(ARTS[,2:21])
MaintPVal <- MaintPVal %>%
  pivot_longer(cols = 1:20, names_to = 'Variable', values_to = 'MaintPVal')

# Create a dataframe of all pvals
PVals <- data.frame('MiniPVal' = MiniPVal$MiniPVal,
                    'SmallPVal' = SmallPVal$SmallPVal,
                    'MediumPVal' = MediumPVal$MediumPVal,
                    'LargePVal' = LargePVal$LargePVal,
                    'DisRepairPVal' = DisRepairPVal$DisRepairPVal,
                    'MaintPVal' = MaintPVal$MaintPVal)

# Bind the pvals dataframe to ARTS_Correlations
ARTS_Correlations <- ARTS_Correlations %>%
  cbind(PVals)

# Reorder ARTS_Correlations so that each p-value column is directly after the correlation value it corresponds to
ARTS_Correlations <- ARTS_Correlations[, c(1:2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13)]

# Remove all intermediate items, these variables will be used again below
rm(Mini, MiniPVal, Small, SmallPVal, Medium, MediumPVal, Large, LargePVal, DisRepair, DisRepairPVal, Maint, MaintPVal, PVals)

# Create a few empty lists, these will be populated in the following for-in loop
MiniPVal <- list()
SmallPVal <- list()
MediumPVal <- list()
LargePVal <- list()
DisRepairPVal <- list()
MaintPVal <- list()

# For each variable in the CES data
for (i in names(CES[,2:11])) {
  # Append the respective list with the output from rcorr(), testing the normalized TotalJobCost against the normalized value from CES
  MiniPVal <- append(MiniPVal, rcorr(AllData$Mini_Normalized, CES[,i]))
  SmallPVal <- append(SmallPVal, rcorr(AllData$Small_Normalized, CES[,i]))
  MediumPVal <- append(MediumPVal, rcorr(AllData$Medium_Normalized, CES[,i]))
  LargePVal <- append(LargePVal, rcorr(AllData$Large_Normalized, CES[,i]))
  DisRepairPVal <- append(DisRepairPVal, rcorr(AllData$DisRepair_Normalized, CES[,i]))
  MaintPVal <- append(MaintPVal, rcorr(AllData$Maint_Normalized, CES[,i]))
  
}

# First convert the list to a dataframe
MiniPVal <- as.data.frame(MiniPVal)
# Select variables that start with 'P.x', these are all of the p-values for the correlation between ARTS variables and the TotalJobCost in question (Mini here)
MiniPVal <- MiniPVal %>%
  select(starts_with(c("P.x")))
# Drop the first row, it is all NA
MiniPVal<- MiniPVal[-1,]
# Conver column names to the appropriate variable name from the ARTS data
colnames(MiniPVal) <- names(CES[,2:11])
# Pivot the dataframe to a long format
MiniPVal <- MiniPVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'MiniPVal')

# All following chunks of code (lines 285-323) follow the same pipeline as above

SmallPVal <- as.data.frame(SmallPVal)
SmallPVal <- SmallPVal %>%
  select(starts_with(c("P.x")))
SmallPVal<- SmallPVal[-1,]
colnames(SmallPVal) <- names(CES[,2:11])
SmallPVal <- SmallPVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'SmallPVal')

MediumPVal <- as.data.frame(MediumPVal)
MediumPVal <- MediumPVal %>%
  select(starts_with(c("P.x")))
MediumPVal<- MediumPVal[-1,]
colnames(MediumPVal) <- names(CES[,2:11])
MediumPVal <- MediumPVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'MediumPVal')

LargePVal <- as.data.frame(LargePVal)
LargePVal <- LargePVal %>%
  select(starts_with(c("P.x")))
LargePVal<- LargePVal[-1,]
colnames(LargePVal) <- names(CES[,2:11])
LargePVal <- LargePVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'LargePVal')

DisRepairPVal <- as.data.frame(DisRepairPVal)
DisRepairPVal <- DisRepairPVal %>%
  select(starts_with(c("P.x")))
DisRepairPVal<- DisRepairPVal[-1,]
colnames(DisRepairPVal) <- names(CES[,2:11])
DisRepairPVal <- DisRepairPVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'DisRepairPVal')

MaintPVal <- as.data.frame(MaintPVal)
MaintPVal <- MaintPVal %>%
  select(starts_with(c("P.x")))
MaintPVal<- MaintPVal[-1,]
colnames(MaintPVal) <- names(CES[,2:11])
MaintPVal <- MaintPVal %>%
  pivot_longer(cols = 1:10, names_to = 'Variable', values_to = 'MaintPVal')

# Create a dataframe of all pvals
PVals <- data.frame('MiniPVal' = MiniPVal$MiniPVal,
                    'SmallPVal' = SmallPVal$SmallPVal,
                    'MediumPVal' = MediumPVal$MediumPVal,
                    'LargePVal' = LargePVal$LargePVal,
                    'DisRepairPVal' = DisRepairPVal$DisRepairPVal,
                    'MaintPVal' = MaintPVal$MaintPVal)

# Bind the pvals dataframe to CES_Correlations
CES_Correlations <- CES_Correlations %>%
  cbind(PVals)

# Reorder CES_Correlations so that each p-value column is directly after the correlation value it corresponds to
CES_Correlations <- CES_Correlations[, c(1:2, 8, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13)]

# Remove all intermediate items
rm(Mini, MiniPVal, Small, SmallPVal, Medium, MediumPVal, Large, LargePVal, DisRepair, DisRepairPVal, Maint, MaintPVal, PVals)

# Output the CER_Correlations and ARTS_Correlations dataframes to an excel file
write.xlsx(CES_Correlations, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/CES_Correlations.xlsx")
write.xlsx(ARTS_Correlations, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/ARTS_Correlations.xlsx")

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

# Normalize all precipitation variables
EastPrecip[, c(2:33)] <- lapply(EastPrecip[, c(2:33)], normalize)

# Create EastPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question 
EastPrecipCor <- as.data.frame(cor(AllData$DisRepair_Normalized, EastPrecip[, c(2:33)]))

# Pivot PrecipCor to a long format
EastPrecipCor <- EastPrecipCor %>%
  pivot_longer(cols = c(1:32))

#Output the Precipitation variable correlation dataframe to an excel file
write.xlsx(EastPrecipCor, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/EastPrecip_Correlations.xlsx")

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
                      (NortheastPrecipData[NortheastPrecipData$Year == i, ][, NEPrecipVars[x]] + NortheastPrecipData[NortheastPrecipData$Year == i-1, ][, NEPrecipVars[x]]) / 2)
    
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

# Normalize all precipitation variables
NEPrecip[, c(2:33)] <- lapply(NEPrecip[, c(2:33)], normalize)

# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question 
NEPrecipCor <- as.data.frame(cor(AllData$DisRepair_Normalized, NEPrecip[, c(2:33)]))

# Pivot NEPrecipCor to a long format
NEPrecipCor <- NEPrecipCor %>%
  pivot_longer(cols = c(1:32))

write.xlsx(EastPrecipCor, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/NEPrecip_Correlations.xlsx")