library(tidyverse)
library(fredr)
library(openxlsx)
library(lubridate)
library(xts)
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Function to calculate percentage difference
percentage_diff <- function(x) {
  diff_x <- c(NA, diff(x))
  percentage_diff_x <- (diff_x / lag(x, n = 1))
  return(percentage_diff_x)
}

ARTSVariables <- fredr_series_search_id('MRTS') %>%
  filter(seasonal_adjustment == 'Not Seasonally Adjusted' & units_short == 'Mil. of $' & frequency == 'Monthly') %>%
  filter(as.Date(observation_end) >= '2021-12-01' & as.Date(observation_start) <= '1993-01-01') %>%
  select(id, title) %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title)) %>%
  mutate(title =  gsub("RetailSales", "", title)) %>%
  filter(id != 'MRTSSM44312USS')

# Create an empty list to store the data frames
dataframes_list <- list()

for (i in 1:length(ARTSVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(ARTSVariables$id[i]), sort_order = 'asc', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
    select(date, value)

  # Set the column name to the AmendedLabel
  col_name <- ARTSVariables$title[i]

  # Store the data frame in the list with the appropriate column name
  dataframes_list[[col_name]] <- data
}

# Combine all data frames into a single data frame
ARTSData <- do.call(cbind, dataframes_list)

# Select only the value columns along with a date column
ARTSData <- ARTSData %>%
  select(RetailTrade.date, ends_with('.value'))

# Rename columns by dropping '.value'
ARTSData <- ARTSData %>%
  rename_with(~gsub(".value", "", .), ends_with('.value'))

# Rename the Date column to read 'Date'
ARTSData <- ARTSData %>%
  rename(Date = RetailTrade.date)

# Store the dates in ARTSDates
ARTSDates <- ARTSData$Date

# Coerce 'Date' to a year-based date variable
ARTSData <- ARTSData %>%
  mutate(Date = year(Date))

# REad in annual CPI data, and select the date/value
Inflation <- fredr(series_id = 'CPIAUCSL', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
  select(date, value)

# Coerce 'date' to a year-based date variable
Inflation <- Inflation %>%
  mutate(date = year(date))

# Store the current CPI value in CPI2021
CPI2021 <- Inflation %>%
  filter(date == 2021) %>%
  select(value)
CPI2021 <- CPI2021$value

# Adjust the CPI to read as multipliers to 2021 dollar amounts (i.e. 1 for 2021, ~1.05 for 2020, etc.)
Inflation <- Inflation %>%
  mutate(value = CPI2021/value) %>%
  rename(Date = date, Rate = value)

# Join the inflation data to the ARTS data
ARTSData <- ARTSData %>%
  left_join(Inflation, by = 'Date')

ARTSCols <- as.numeric(ncol(ARTSData))-1
# Inflation adjust the ARTS data
ARTSData[,2:ARTSCols] <- ARTSData[,2:ARTSCols]*ARTSData$Rate

# Drop the inflation rate
ARTSData <- ARTSData %>%
  select(-Rate)
rm(ARTSCols)
# The following code follows the same logic as above to grab data from outside sources

OtherVariables <- fredr_series_search_text(search_text = 'Monthly', filter_variable = 'seasonal_adjustment')
OtherVariables <- OtherVariables %>%
  filter(frequency %in% c('Monthly', 'Weekly', 'Daily') & units_short %in% c('Mil. of $', 'Bil. of U.S. $', 'Bil. of $') & seasonal_adjustment == 'Not Seasonally Adjusted') %>%
  mutate(observation_end = as.Date(observation_end),
         observation_start = as.Date(observation_start)) %>%
  filter(observation_end >= '2023-01-01' & observation_start <= '1993-01-01') %>%
  select(id, title) %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title))

# Create an empty list to store the data frames
dataframes_list <- list()

for (i in 1:length(OtherVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(OtherVariables$id[i]), sort_order = 'asc', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
    select(date, value)

  # Set the column name to the AmendedLabel
  col_name <- OtherVariables$title[i]

  # Store the data frame in the list with the appropriate column name
  dataframes_list[[col_name]] <- data
}

# Combine all data frames into a single data frame
OtherData <- do.call(cbind, dataframes_list)

# Select only the value columns along with a date column
OtherData <- OtherData %>%
  select(M2.date, ends_with('.value'))

# Rename columns by dropping '.value'
OtherData <- OtherData %>%
  rename_with(~gsub(".value", "", .), ends_with('.value'))

OtherData <- OtherData %>%
  rename(Date = M2.date)

OtherDates <- OtherData$Date

OtherData <- OtherData %>%
  mutate(Date = year(Date))

OtherData <- OtherData %>%
  left_join(Inflation, by = 'Date')

OtherDataCols <- as.numeric(ncol(OtherData))-1

OtherData[,2:OtherDataCols] <- OtherData[,2:OtherDataCols]*OtherData$Rate

OtherData <- OtherData %>%
  select(-Rate)
rm(OtherDataCols)

ConstructionVariables <- fredr_series_search_text('construction') %>%
  filter(seasonal_adjustment == 'Not Seasonally Adjusted' & units_short %in% c('Mil. of $', 'Bil. of US $', 'Thous. of Units') & frequency %in% c('Monthly', 'Quarterly', 'Weekly')) %>%
  filter(as.Date(observation_end) >= '2021-12-01' & as.Date(observation_start) <= '1993-01-01') %>%
  #select(id, title) %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title)) %>%
  mutate(title =  gsub("RetailSales", "", title)) %>%
  filter(id != 'MRTSSM44312USS')

# Create an empty list to store the data frames
dataframes_list <- list()

for (i in 1:length(ConstructionVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(ConstructionVariables$id[i]), sort_order = 'asc', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
    select(date, value)

  # Set the column name to the AmendedLabel
  col_name <- ConstructionVariables$title[i]

  # Store the data frame in the list with the appropriate column name
  dataframes_list[[col_name]] <- data
}

# Combine all data frames into a single data frame
ConstructionData <- do.call(cbind, dataframes_list)

# Select only the value columns along with a date column
ConstructionData <- ConstructionData %>%
  select(NewPrivately_OwnedHousingUnitsUnderConstructionTotalUnits.date, ends_with('.value'))

# Rename columns by dropping '.value'
ConstructionData <- ConstructionData %>%
  rename_with(~gsub(".value", "", .), ends_with('.value'))

ConstructionData <- ConstructionData %>%
  rename(Date = NewPrivately_OwnedHousingUnitsUnderConstructionTotalUnits.date)

ConstructionDates <- ConstructionData$Date

ConstructionData$Date <- year(ConstructionData$Date)

ConstructionData <- ConstructionData %>%
  left_join(Inflation, by = 'Date')

ConstructionData[,c(2, 6:12, 17)] <- ConstructionData[,c(2, 6:12, 17)]*ConstructionData$Rate

ConstructionData <- ConstructionData %>%
  select(-Rate)


LumberVariables <- fredr_series_search_text('lumber') %>%
  filter(seasonal_adjustment == 'Not Seasonally Adjusted' & frequency == 'Monthly' & as.Date(observation_start) <= '1993-01-01' & str_detect(units, 'Index') & as.Date(observation_end) >= '2021-01-01') %>%
  select(id, title) %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title))

# Create an empty list to store the data frames
dataframes_list <- list()

for (i in 1:length(LumberVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(LumberVariables$id[i]), sort_order = 'asc', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
    select(date, value)

  # Set the column name to the AmendedLabel
  col_name <- LumberVariables$title[i]

  # Store the data frame in the list with the appropriate column name
  dataframes_list[[col_name]] <- data
}

# Combine all data frames into a single data frame
LumberData <- do.call(cbind, dataframes_list)

# Select only the value columns along with a date column
LumberData <- LumberData %>%
  select(ProducerPriceIndexbyCommodityPulp_Paper_andAlliedProductsWoodPulp.date, ends_with('.value'))

# Rename columns by dropping '.value'
LumberData <- LumberData %>%
  rename_with(~gsub(".value", "", .), ends_with('.value'))

LumberData <- LumberData %>%
  rename(Date = ProducerPriceIndexbyCommodityPulp_Paper_andAlliedProductsWoodPulp.date)

LumberDates <- LumberData$Date

LumberData <- LumberData %>%
  mutate(Date = year(Date))

LumberData <- LumberData %>%
  left_join(Inflation, by = 'Date')

LumberDataCols <- as.numeric(ncol(LumberData))-1

LumberData[,2:LumberDataCols] <- LumberData[,2:LumberDataCols]*LumberData$Rate

LumberData <- LumberData %>%
  select(-Rate)


OutsideData <- ARTSData %>%
  left_join(ConstructionData, by = 'Date')
OutsideData <- OutsideData %>%
  left_join(OtherData, by = 'Date')


OutsideDataVariables <- names(OutsideData[-1])
OutsideData <- OutsideData %>%
  filter(Date >= 1994)
Years <- c(seq(1995, 2021, 2))

OutsideDataList <- list()

# For each AHS year
for (x in 1:length(OutsideDataVariables)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    OutsideDataList <- append(OutsideDataList,
                       (OutsideData[OutsideData$Date == i, ][, OutsideDataVariables[x]] + OutsideData[OutsideData$Date == i-1, ][, OutsideDataVariables[x]]))
  }
}

# Convert ARTSList to a dataframe
OutsideDataList <- as.data.frame(OutsideDataList)
# Pivot ARTSList to a long format
OutsideDataList <- OutsideDataList %>%
  pivot_longer(cols = 1:ncol(OutsideDataList))


# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
OutsideDataTemplate <- data.frame(matrix(NA, nrow = 14, ncol = ncol(OutsideData)))
# Transfer variable names to column names for ARTS 2-year data
colnames(OutsideDataTemplate) <- OutsideDataVariables

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:ncol(OutsideData)) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 14 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 14
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  OutsideDataTemplate[, i] <- OutsideDataList[start_row:end_row, 2, drop = FALSE]
}

OutsideData <- OutsideDataTemplate

# Lumber Conversion to annual

LumberVariables <- names(LumberData[-1])
LumberData <- LumberData %>%
  filter(Date >= 1994)

LumberDataList <- list()

# For each AHS year
for (x in 1:length(LumberVariables)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    LumberDataList <- append(LumberDataList,
                              (LumberData[LumberData$Date == i, ][, LumberVariables[x]] + LumberData[LumberData$Date == i-1, ][, LumberVariables[x]]) / 2)
  }
}

# Convert ARTSList to a dataframe
LumberDataList <- as.data.frame(LumberDataList)
# Pivot ARTSList to a long format
LumberDataList <- LumberDataList %>%
  pivot_longer(cols = 1:ncol(LumberDataList))


# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
LumberDataTemplate <- data.frame(matrix(NA, nrow = 14, ncol = length(LumberVariables)))
# Transfer variable names to column names for ARTS 2-year data
colnames(LumberDataTemplate) <- LumberVariables

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:length(LumberVariables)) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 14 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 14
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  LumberDataTemplate[, i] <- LumberDataList[start_row:end_row, 2, drop = FALSE]
}

LumberData <- LumberDataTemplate

BurnsData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/Burns_Data_Download.xlsx")
BurnsVariables <- names(BurnsData[-1])

BurnsDataList <- list()

# For each AHS year
for (x in 1:length(BurnsVariables)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    BurnsDataList <- append(BurnsDataList,
                             (BurnsData[BurnsData$Date == i, ][, BurnsVariables[x]] + BurnsData[BurnsData$Date == i-1, ][, BurnsVariables[x]]) / 2)
  }
}

# Convert ARTSList to a dataframe
BurnsDataList <- as.data.frame(BurnsDataList)
# Pivot ARTSList to a long format
BurnsDataList <- BurnsDataList %>%
  pivot_longer(cols = 1:ncol(BurnsDataList))


# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
BurnsDataTemplate <- data.frame(matrix(NA, nrow = 14, ncol = length(BurnsVariables)))
# Transfer variable names to column names for ARTS 2-year data
colnames(BurnsDataTemplate) <- BurnsVariables

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:length(BurnsVariables)) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 14 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 14
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  BurnsDataTemplate[, i] <- BurnsDataList[start_row:end_row, 2, drop = FALSE]
}
BurnsData <- BurnsDataTemplate


PermitData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/JohnBurns_PermitCounts_20230717.xlsx")
PermitDates <- unique(PermitData$Year)
PermitData <- PermitData %>%
  mutate(Date = paste0(Year, Quarter),
         Date = as.yearqtr(Date)) %>%
  select(-c(Year,Quarter))

PermitData <- as.xts(PermitData)
endpoints <- endpoints(PermitData,on = 'years')

#Create an empty list to store the yearly sums for each column
yearly_sums <- list()

# Loop through each column of HIRLData and calculate the yearly sum
for (col in 1:ncol(PermitData)) {
  yearly_sum <- period.apply(PermitData[, col], INDEX = endpoints, FUN = sum)
  yearly_sums[[col]] <- yearly_sum
}
PermitData <- as.data.frame(yearly_sums)
PermitData$Date <- PermitDates
PermitData <- PermitData[, c(ncol(PermitData), 1:ncol(PermitData)-1)]

HIRLData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/HIRLData.xlsx", sheet = 1)
HIRLDates <- unique(HIRLData$Year)
HIRLData <- HIRLData %>%
  mutate(Date = paste0(Year, 'Q', Quarter),
         Date = as.yearqtr(Date)) %>%
  select(-c(Year,Quarter))

HIRLData <- as.xts(HIRLData)
endpoints <- endpoints(HIRLData,on = 'years')

#Create an empty list to store the yearly sums for each column
yearly_sums <- list()

# Loop through each column of HIRLData and calculate the yearly sum
for (col in 1:ncol(HIRLData)) {
  yearly_sum <- period.apply(HIRLData[, col], INDEX = endpoints, FUN = sum)
  yearly_sums[[col]] <- yearly_sum
}
HIRLData <- as.data.frame(yearly_sums)
HIRLData$Date <- HIRLDates
HIRLData <- HIRLData[, c(ncol(HIRLData), 1:ncol(HIRLData)-1)]
names(HIRLData) <- paste0(names(HIRLData), '_Total')
HIRLData <- HIRLData %>%
  rename(Date = Date_Total)

Permit_HIRL_Data <- PermitData %>%
  left_join(HIRLData, by = 'Date')

HIRLData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/HIRLData.xlsx", sheet = 2)
HIRLDates <- unique(HIRLData$Year)
HIRLData <- HIRLData %>%
  mutate(Date = paste0(Year, 'Q', Quarter),
         Date = as.yearqtr(Date)) %>%
  select(-c(Year,Quarter))

HIRLData <- as.xts(HIRLData)
endpoints <- endpoints(HIRLData,on = 'years')

#Create an empty list to store the yearly sums for each column
yearly_sums <- list()

# Loop through each column of HIRLData and calculate the yearly sum
for (col in 1:ncol(HIRLData)) {
  yearly_sum <- period.apply(HIRLData[, col], INDEX = endpoints, FUN = sum)
  yearly_sums[[col]] <- yearly_sum
}
HIRLData <- as.data.frame(yearly_sums)
HIRLData$Date <- HIRLDates
HIRLData <- HIRLData[, c(ncol(HIRLData), 1:ncol(HIRLData)-1)]
names(HIRLData) <- paste0(names(HIRLData), '_RR')
HIRLData <- HIRLData %>%
  rename(Date = Date_RR)

Permit_HIRL_Data <- Permit_HIRL_Data %>%
  left_join(HIRLData, by = 'Date')

Permit_HIRL_Variables <- names(Permit_HIRL_Data[-1])

Permit_HIRL_DataList <- list()

Years <- c(seq(2001,2021,2))

# For each AHS year
for (x in 1:length(Permit_HIRL_Variables)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    Permit_HIRL_DataList <- append(Permit_HIRL_DataList,
                            (Permit_HIRL_Data[Permit_HIRL_Data$Date == i, ][, Permit_HIRL_Variables[x]] + Permit_HIRL_Data[Permit_HIRL_Data$Date == i-1, ][, Permit_HIRL_Variables[x]]))
  }
}

# Convert ARTSList to a dataframe
Permit_HIRL_DataList <- as.data.frame(Permit_HIRL_DataList)
# Pivot ARTSList to a long format
Permit_HIRL_DataList <- Permit_HIRL_DataList %>%
  pivot_longer(cols = 1:ncol(Permit_HIRL_DataList))


# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
Permit_HIRL_Template <- data.frame(matrix(NA, nrow = 11, ncol = length(Permit_HIRL_Variables)))
# Transfer variable names to column names for ARTS 2-year data
colnames(Permit_HIRL_Template) <- Permit_HIRL_Variables

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:length(Permit_HIRL_Variables)) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 11 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 11
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  Permit_HIRL_Template[, i] <- Permit_HIRL_DataList[start_row:end_row, 2, drop = FALSE]
}

Permit_HIRL_Data <- Permit_HIRL_Template


rm(ARTSData, ARTSVariables, BurnsDataList, BurnsDataTemplate, BurnsVariables, ConstructionData, ConstructionVariables, HIRLData, PermitData, dataframes_list, LumberDataList,
   data, LumberDataTemplate, LumberVariables, yearly_sum, yearly_sums, OutsideDataTemplate, OutsideDataList, ARTSDates, ConstructionDates, LumberDates, OtherDates, OtherVariables,
   OtherData, PermitDates, Permit_HIRL_DataList, Permit_HIRL_Template)


BurnsData$Date <- c(seq(1995, 2021, 2))
BurnsData <- BurnsData[,c(ncol(BurnsData), 1:ncol(BurnsData)-1)]
LumberData$Date <- c(seq(1995, 2021, 2))
LumberData <- LumberData[,c(ncol(LumberData), 1:ncol(LumberData)-1)]
OutsideData$Date <- c(seq(1995, 2021, 2))
OutsideData <- OutsideData[,c(ncol(OutsideData), 1:ncol(OutsideData)-1)]
Permit_HIRL_Data$Date <- c(seq(2001, 2021, 2))
Permit_HIRL_Data <- Permit_HIRL_Data[,c(ncol(Permit_HIRL_Data), 1:ncol(Permit_HIRL_Data)-1)]

VisaData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/VisaData.xlsx")
VisaData <- VisaData %>%
  mutate(Date = year(Date))

VisaData <- VisaData %>%
  mutate(AHSYear = case_when(Date %in% c(2014,2015) ~ 2015,
                             Date %in% c(2016,2017) ~ 2017,
                             Date %in% c(2018, 2019) ~ 2019,
                             Date %in% c(2020,2021) ~ 2021)) %>%
  filter(!is.na(AHSYear))

VisaData <- VisaData %>%
  group_by(AHSYear) %>%
  summarize(SMI = mean(`Non-Seasonally.adjusted`, na.rm = TRUE))

VisaData <- VisaData %>%
  rename(Date = AHSYear)



# Create a 'Years' column as a date
LumberData$Date <- as.Date(paste0(LumberData$Date, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
LumberData <- as.xts(LumberData)
# Create AllDataDiff, this will contain 2-year growth rates.
LumberData <- diff(LumberData) / dplyr::lag(LumberData, n = 1)
LumberData <- as.data.frame(LumberData)
LumberData$Date <- c(seq(1995,2021,2))
LumberData <- LumberData[,c(ncol(LumberData), 1:ncol(LumberData)-1)]
LumberData <- LumberData %>%
  filter(Date >= 1999)

OutsideData$Date <- c(seq(1995, 2021,2))
# Create a 'Years' column as a date
OutsideData$Date <- as.Date(paste0(OutsideData$Date, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
OutsideData <- as.xts(OutsideData)
# Create AllDataDiff, this will contain 2-year growth rates.
OutsideData <- diff(OutsideData) / dplyr::lag(OutsideData, n = 1)
OutsideData <- as.data.frame(OutsideData)
OutsideData$Date <- c(seq(1995,2021,2))
OutsideData <- OutsideData[,c(ncol(OutsideData), 1:ncol(OutsideData)-1)]
OutsideData <- OutsideData[,1:102]
OutsideData <- OutsideData %>%
  filter(Date >= 1999)

# Create a 'Years' column as a date
Permit_HIRL_Data$Date <- as.Date(paste0(Permit_HIRL_Data$Date, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
Permit_HIRL_Data <- as.xts(Permit_HIRL_Data)
# Create AllDataDiff, this will contain 2-year growth rates.
Permit_HIRL_Data <- diff(Permit_HIRL_Data) / dplyr::lag(Permit_HIRL_Data, n = 1)
Permit_HIRL_Data <- as.data.frame(Permit_HIRL_Data)
Permit_HIRL_Data$Date <- c(seq(2001,2021,2))
Permit_HIRL_Data <- Permit_HIRL_Data[,c(ncol(Permit_HIRL_Data), 1:ncol(Permit_HIRL_Data)-1)]
Permit_HIRL_Data <- Permit_HIRL_Data %>%
  filter(Date >= 2003)

# Create a 'Years' column as a date
VisaData$Date <- as.Date(paste0(VisaData$Date, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
VisaData <- as.xts(VisaData)
# Create AllDataDiff, this will contain 2-year growth rates.
VisaData <- diff(VisaData) / dplyr::lag(VisaData, n = 1)
VisaData <- as.data.frame(VisaData)
VisaData$Date <- c(2015, 2017, 2019, 2021)
VisaData <- VisaData[,c(2, 1)]
VisaData <- VisaData %>%
  filter(Date >= 2017)


BurnsData <- BurnsData %>%
  filter(Date >= 1999)
#######
# Filters for 1995-2009 correlations!
OutsideData <- OutsideData %>%
  filter(Date < 2011)
LumberData <- LumberData %>%
  filter(Date < 2011)
BurnsData <- BurnsData %>%
  filter(Date < 2011)
Permit_HIRL_Data <- Permit_HIRL_Data %>%
  filter(Date < 2011)

# Read in the AHS Panel and filter for 1997-current (issues with 1995 data)
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx") %>%
  filter(Year >= 1995)

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


# Define a 'Years' variable
Years <- c(seq(1995, 2021, 2))


# Create 'AllData' containing the TotalJobCost for each of the Cost Classes + DisRepairs/Maintanence
AllData <- data.frame(Year = Years,
                      Mini = Mini$TotalJobCost,
                      Small = Small$TotalJobCost,
                      Medium = Medium$TotalJobCost,
                      Large = Large$TotalJobCost,
                      DisRepair = DisRepair$TotalJobCost)

rm(Mini, Small, Medium, Large, DisRepair)

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

AllData <- AllData[, c(11, 1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]
AllData <- AllData %>%
  rename(MiniDiff = Mini.1, SmallDiff = Small.1, MediumDiff = Medium.1, LargeDiff = Large.1, DisRepairDiff = DisRepair.1)

# EDIT THIS TO BE >= 1995 if running correlations on all years!
AllData <- AllData %>%
  filter(Year < 2011 & Year >= 1999)


# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, OutsideData[, c(2:ncol(OutsideData))]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:ncol(OutsideData)-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, OutsideData[, c(2:ncol(OutsideData))]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:ncol(OutsideData)-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, OutsideData[, c(2:ncol(OutsideData))]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:ncol(OutsideData)-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, OutsideData[, c(2:ncol(OutsideData))]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:ncol(OutsideData)-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, OutsideData[, c(2:ncol(OutsideData))]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:ncol(OutsideData)-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
OutsideData_Correlations <- data.frame(Variable = MiniCor$name,
                                Mini = MiniCor$value,
                                Small = SmallCor$value,
                                Medium = MediumCor$value,
                                Large = LargeCor$value,
                                DisRepair = DisRepairCor$value)



MiniCor <- as.data.frame(cor(AllData$MiniDiff, BurnsData[, c(2:ncol(BurnsData))]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:ncol(BurnsData)-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, BurnsData[, c(2:ncol(BurnsData))]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:ncol(BurnsData)-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, BurnsData[, c(2:ncol(BurnsData))]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:ncol(BurnsData)-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, BurnsData[, c(2:ncol(BurnsData))]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:ncol(BurnsData)-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, BurnsData[, c(2:ncol(BurnsData))]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:ncol(BurnsData)-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
BurnsData_Correlations <- data.frame(Variable = MiniCor$name,
                                            Mini = MiniCor$value,
                                            Small = SmallCor$value,
                                            Medium = MediumCor$value,
                                            Large = LargeCor$value,
                                            DisRepair = DisRepairCor$value)



MiniCor <- as.data.frame(cor(AllData$MiniDiff, LumberData[, c(2:ncol(LumberData))]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:ncol(LumberData)-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, LumberData[, c(2:ncol(LumberData))]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:ncol(LumberData)-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, LumberData[, c(2:ncol(LumberData))]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:ncol(LumberData)-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, LumberData[, c(2:ncol(LumberData))]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:ncol(LumberData)-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, LumberData[, c(2:ncol(LumberData))]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:ncol(LumberData)-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
LumberData_Correlations <- data.frame(Variable = MiniCor$name,
                                     Mini = MiniCor$value,
                                     Small = SmallCor$value,
                                     Medium = MediumCor$value,
                                     Large = LargeCor$value,
                                     DisRepair = DisRepairCor$value)

AllData <- AllData %>%
  filter(Year >= 2003)

# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, Permit_HIRL_Data[, c(2:ncol(Permit_HIRL_Data))]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:ncol(Permit_HIRL_Data)-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, Permit_HIRL_Data[, c(2:ncol(Permit_HIRL_Data))]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:ncol(Permit_HIRL_Data)-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, Permit_HIRL_Data[, c(2:ncol(Permit_HIRL_Data))]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:ncol(Permit_HIRL_Data)-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, Permit_HIRL_Data[, c(2:ncol(Permit_HIRL_Data))]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:ncol(Permit_HIRL_Data)-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, Permit_HIRL_Data[, c(2:ncol(Permit_HIRL_Data))]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:ncol(Permit_HIRL_Data)-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
Permit_HIRL_Data_Correlations <- data.frame(Variable = MiniCor$name,
                                       Mini = MiniCor$value,
                                       Small = SmallCor$value,
                                       Medium = MediumCor$value,
                                       Large = LargeCor$value,
                                       DisRepair = DisRepairCor$value)

# AllData <- AllData %>%
#   filter(Year >= 2017)
#
# MiniCor <- as.data.frame(cor(AllData$MiniDiff, VisaData[, 2]))
# MiniCor <- MiniCor %>%
#   pivot_longer(cols = 1)
#
# SmallCor <- as.data.frame(cor(AllData$SmallDiff, VisaData[, 2]))
# SmallCor <- SmallCor %>%
#   pivot_longer(cols = 1)
#
# MediumCor <- as.data.frame(cor(AllData$MediumDiff, VisaData[, 2]))
# MediumCor <- MediumCor %>%
#   pivot_longer(cols = 1)
#
# LargeCor <- as.data.frame(cor(AllData$LargeDiff, VisaData[, 2]))
# LargeCor <- LargeCor %>%
#   pivot_longer(cols = 1)
#
# DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, VisaData[, 2]))
# DisRepairCor <- DisRepairCor %>%
#   pivot_longer(cols = 1)
#
# # Condense all correlations into one dataframe called 'ARTS_Correlations'
# Visa_Correlations <- data.frame(Variable = 'Visa_SMI',
#                                             Mini = MiniCor$value,
#                                             Small = SmallCor$value,
#                                             Medium = MediumCor$value,
#                                             Large = LargeCor$value,
#                                             DisRepair = DisRepairCor$value)


Correlations <- OutsideData_Correlations %>%
  rbind(BurnsData_Correlations) %>%
  rbind(LumberData_Correlations)
  #rbind(Visa_Correlations)


ApplianceVariables <- fredr_series_search_text('appliances by income')
ApplianceVariables <- ApplianceVariables %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

ApplianceVariables2 <- fredr_series_search_text('appliances by housing tenure')
ApplianceVariables2 <- ApplianceVariables2 %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

ApplianceVariables3 <- fredr_series_search_text('housewares by housing tenure')
ApplianceVariables3 <- ApplianceVariables3 %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

FloorCoveringVariables <- fredr_series_search_text('floor covering by income')
FloorCoveringVariables <- FloorCoveringVariables %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

MaintenanceVariables <- fredr_series_search_text('Maintenance owned dwelling by Income Quintiles')
MaintenanceVariables <- MaintenanceVariables %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

MaintenanceVariables2 <- fredr_series_search_text('Maintenance owned dwelling by Housing Tenure')
MaintenanceVariables2 <- MaintenanceVariables2 %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

FurnitureVariables <- fredr_series_search_text('Furniture by Income quintile')
FurnitureVariables <- FurnitureVariables %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

FurnitureVariables2 <- fredr_series_search_text('Furniture by Housing Tenure')
FurnitureVariables2 <- FurnitureVariables2 %>%
  filter(as.Date(observation_start) <= '1994-01-01' & as.Date(observation_end) >= '2021-01-01' & units_short == 'U.S. $')

CESVariables <- ApplianceVariables %>%
  rbind(ApplianceVariables2, ApplianceVariables3, FloorCoveringVariables, MaintenanceVariables,
        MaintenanceVariables2, FurnitureVariables, FurnitureVariables2)

CESVariables <- CESVariables %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "_", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title)) %>%
  select(id, title)


dataframes_list <- list()


for (i in 1:length(CESVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(CESVariables$id[i]), sort_order = 'asc', frequency = 'a', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-01-01')) %>%
    select(date, value)

  # Set the column name to the AmendedLabel
  col_name <- CESVariables$title[i]

  # Store the data frame in the list with the appropriate column name
  dataframes_list[[col_name]] <- data
}

# Combine all data frames into a single data frame
CESData <- do.call(cbind, dataframes_list)

# Select only the value columns along with a date column
CESData <- CESData %>%
  select(`Expenditures_SmallAppliances_MiscellaneousHousewaresbyQuintilesofIncomeBeforeTaxes_Lowest20Percent(1stto20thPercentile).date`, ends_with('.value'))

# Rename columns by dropping '.value'
CESData <- CESData %>%
  rename_with(~gsub(".value", "", .), ends_with('.value'))

CESData <- CESData %>%
  rename(Date = `Expenditures_SmallAppliances_MiscellaneousHousewaresbyQuintilesofIncomeBeforeTaxes_Lowest20Percent(1stto20thPercentile).date`)

CESDates <- CESData$Date

CESData <- CESData %>%
  mutate(Date = year(Date))

CESData <- CESData %>%
  left_join(Inflation, by = 'Date')

CESCols <- as.numeric(ncol(CESData))-1
CESData[,2:CESCols] <- CESData[,2:CESCols]*CESData$Rate

CESData <- CESData %>%
  select(-Rate)




CESDataVariables <- names(CESData[-1])
CESData <- CESData %>%
  filter(Date >= 1994)
Years <- c(seq(1995, 2021, 2))

CESDataList <- list()

# For each AHS year
for (x in 1:length(CESDataVariables)) {
  for (i in Years) {
    # Append to 'ARTSList' the following: Current Year + Prior Year
    CESDataList <- append(CESDataList,
                              (CESData[CESData$Date == i, ][, CESDataVariables[x]] + CESData[CESData$Date == i-1, ][, CESDataVariables[x]]) / 2)
  }
}

# Convert ARTSList to a dataframe
CESDataList <- as.data.frame(CESDataList)
# Pivot ARTSList to a long format
CESDataList <- CESDataList %>%
  pivot_longer(cols = 1:ncol(CESDataList))


# Create a template dataframe for ARTS 2-year data, nrow = 13 for 13 AHS years (1997-2021), ncol = 20 for 20 ARTS variables
CESDataTemplate <- data.frame(matrix(NA, nrow = 14, ncol = length(CESDataVariables)))
# Transfer variable names to column names for ARTS 2-year data
colnames(CESDataTemplate) <- CESDataVariables

# For each number 1-20 (20 = number of ARTS variables)
for (i in 1:70) {
  # Define a start row (1 when i=1, 14 when i=2, etc.)
  start_row <- (i - 1) * 14 + 1
  # Define an end row (13 when i = 1, 26 when i=2, etc.)
  end_row <- i * 14
  # Paste the values within start_row/end_row into the ith column in ARTS...This will place each variable's value into a seperate column.
  CESDataTemplate[, i] <- CESDataList[start_row:end_row, 2, drop = FALSE]
}

CESData <- CESDataTemplate
CESData$Date <- c(seq(1995, 2021, 2))
CESData <- CESData[, c(ncol(CESData), 1:ncol(CESData)-1)]

# Create a 'Years' column as a date
CESData$Date <- as.Date(paste0(CESData$Date, "-01-01", sep = ""), format = "%Y-%m-%d")
# Convert All_Data to an xts object
CESData <- as.xts(CESData)
# Create AllDataDiff, this will contain 2-year growth rates.
CESData <- diff(CESData) / dplyr::lag(CESData, n = 1)
CESData <- as.data.frame(CESData)
CESData$Date <- c(seq(1995,2021,2))
CESData <- CESData[, c(ncol(CESData), 1:ncol(CESData)-1)]
CESData <- CESData %>%
  filter(Date < 2011 & Date >= 1999)



# Read in the AHS Panel and filter for 1997-current (issues with 1995 data)
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx") %>%
  filter(Year >= 1995)

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


# Define a 'Years' variable
Years <- c(seq(1995, 2021, 2))


# Create 'AllData' containing the TotalJobCost for each of the Cost Classes + DisRepairs/Maintanence
AllData <- data.frame(Year = Years,
                      Mini = Mini$TotalJobCost,
                      Small = Small$TotalJobCost,
                      Medium = Medium$TotalJobCost,
                      Large = Large$TotalJobCost,
                      DisRepair = DisRepair$TotalJobCost)

rm(Mini, Small, Medium, Large, DisRepair)

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

AllData <- AllData[, c(11, 1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]
AllData <- AllData %>%
  rename(MiniDiff = Mini.1, SmallDiff = Small.1, MediumDiff = Medium.1, LargeDiff = Large.1, DisRepairDiff = DisRepair.1)

# EDIT THIS TO BE >= 1995 if running correlations on all years!
AllData <- AllData %>%
  filter(Year < 2011 & Year >= 1999)

# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, CESData[, c(2:ncol(CESData))]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:ncol(CESData)-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, CESData[, c(2:ncol(CESData))]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:ncol(CESData)-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, CESData[, c(2:ncol(CESData))]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:ncol(CESData)-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, CESData[, c(2:ncol(CESData))]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:ncol(CESData)-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, CESData[, c(2:ncol(CESData))]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:ncol(CESData)-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
CES_Correlations <- data.frame(Variable = MiniCor$name,
                                       Mini = MiniCor$value,
                                       Small = SmallCor$value,
                                       Medium = MediumCor$value,
                                       Large = LargeCor$value,
                                       DisRepair = DisRepairCor$value)

Correlations <- Correlations %>%
  rbind(CES_Correlations)


# Read in total precipitation (US East) data
EastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation.xlsx")

# Define the years to analyze
Years <- c(seq(1997, 2021, 2))

# Store variable names in 'EastPrecipVars'
EastPrecipVars <- names(EastPrecipData[, 2:ncol(EastPrecipData)])

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
  pivot_longer(cols = 1:ncol(EastPrecipList))

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
EastPrecip <- data.frame(matrix(NA, nrow = 13, ncol = length(EastPrecipVars)))
# Populate column names for the empty template with variable names (stored in 'EastPrecipVars')
colnames(EastPrecip) <- EastPrecipVars

# for each number in 1-32
for (i in 1:length(EastPrecipVars)) {
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
EastPrecip <- EastPrecip[, c(length(EastPrecipVars)+1, 1:length(EastPrecipVars))]


EastPrecip$Year <- as.Date(paste0(EastPrecip$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecip <- as.xts(EastPrecip)

# Calculate the growth rate manually using diff
EastPrecipDiff <- diff(EastPrecip) / dplyr::lag(EastPrecip, n = 1)
EastPrecipDiff <- as.data.frame(EastPrecipDiff)
EastPrecipDiff$Year <- Years
EastPrecipDiff <- EastPrecipDiff[, c(length(EastPrecipVars)+1, 1:length(EastPrecipVars))]
EastPrecipDiff <- EastPrecipDiff %>%
  filter(Year < 2011 & Year >= 1999)

AllData <- AllData %>%
  filter(Year < 2011 & Year >= 1999)
# Create EastPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question
EastPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, EastPrecipDiff[, c(2:ncol(EastPrecipDiff))]))

# Pivot PrecipCor to a long format
EastPrecipCor <- EastPrecipCor %>%
  pivot_longer(cols = c(1:ncol(EastPrecipCor)))

# Read in total precipitation (US Northeast) data
NortheastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USNortheast_TotalPrecipitation.xlsx")

# Store variable names in 'NEPrecipVars'
NEPrecipVars <- names(NortheastPrecipData[, 2:ncol(NortheastPrecipData)])
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
  pivot_longer(cols = 1:ncol(NEPrecipList))

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
NEPrecip <- data.frame(matrix(NA, nrow = 13, ncol = length(NEPrecipVars)))
# Populate column names for the empty template with variable names (stored in 'NEPrecipVars')
colnames(NEPrecip) <- NEPrecipVars

# for each number in 1-32
for (i in 1:length(NEPrecipVars)) {
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
NEPrecip <- NEPrecip[, c(ncol(NEPrecip), 1:ncol(NEPrecip)-1)]

NEPrecip$Year <- as.Date(paste0(NEPrecip$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
NEPrecip <- as.xts(NEPrecip)

# Calculate the growth rate manually using diff
NEPrecipDiff <- diff(NEPrecip) / dplyr::lag(NEPrecip, n = 1)
NEPrecipDiff <- as.data.frame(NEPrecipDiff)
NEPrecipDiff$Year <- Years
NEPrecipDiff <- NEPrecipDiff[, c(ncol(NEPrecipDiff), 1:ncol(NEPrecipDiff)-1)]
NEPrecipDiff <- NEPrecipDiff %>%
  filter(Year < 2011 & Year >= 1999)


# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question
NEPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, NEPrecipDiff[, c(2:ncol(NEPrecipDiff))]))

# Pivot NEPrecipCor to a long format
NEPrecipCor <- NEPrecipCor %>%
  pivot_longer(cols = c(1:ncol(NEPrecipCor)))

# Read in total precipitation (US Northeast) data
EastPrecip_RCP4.5 <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation_RCP4.5.xlsx")

# Store variable names in 'NEPrecipVars'
EastPrecipRCPVars <- names(EastPrecip_RCP4.5[, 2:ncol(EastPrecip_RCP4.5)])
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
  pivot_longer(cols = 1:ncol(EastPrecipRCPList))

# Create an empty template dataframe (nrow = 13 for 13 AHS Years, ncol = 32 for 32 precipitation variables)
EastPrecipRCP <- data.frame(matrix(NA, nrow = 13, ncol = length(EastPrecipRCPVars)))
# Populate column names for the empty template with variable names (stored in 'NEPrecipVars')
colnames(EastPrecipRCP) <- EastPrecipRCPVars

# for each number in 1-32
for (i in 1:length(EastPrecipRCPVars)) {
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
EastPrecipRCP <- EastPrecipRCP[, c(ncol(EastPrecipRCP), 1:ncol(EastPrecipRCP)-1)]



EastPrecipRCP$Year <- as.Date(paste0(EastPrecipRCP$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecipRCP <- as.xts(EastPrecipRCP)

# Calculate the growth rate manually using diff
EastPrecipRCPDiff <- diff(EastPrecipRCP) / dplyr::lag(EastPrecipRCP, n = 1)
EastPrecipRCPDiff <- as.data.frame(EastPrecipRCPDiff)
EastPrecipRCPDiff$Year <- Years
EastPrecipRCPDiff <- EastPrecipRCPDiff[, c(ncol(EastPrecipRCPDiff), 1:ncol(EastPrecipRCPDiff)-1)]
EastPrecipRCPDiff <- EastPrecipRCPDiff %>%
  filter(Year < 2011 & Year >= 1999)

# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question
EastPrecipRCPCor <- as.data.frame(cor(AllData$DisRepairDiff, EastPrecipRCPDiff[, c(2:ncol(EastPrecipRCPDiff))]))

# Pivot NEPrecipCor to a long format
EastPrecipRCPCor <- EastPrecipRCPCor %>%
  pivot_longer(cols = c(1:ncol(EastPrecipRCPCor)))

EastPrecipCor$Source <- 'CR_EastUS'
NEPrecipCor$Source <- 'CR_NEUS'
EastPrecipRCPCor$Source <- 'CR_EastUS_RCP4.5'

Precip_Correlations <- EastPrecipCor %>%
  rbind(NEPrecipCor, EastPrecipRCPCor)

Precip_Correlations <- Precip_Correlations %>%
  mutate(name = paste0(name, '_', Source)) %>%
  rename(Variable = name, DisRepair = value) %>%
  select(-Source)

Correlations <- Correlations %>%
  plyr::rbind.fill(Precip_Correlations)

Correlations <- Correlations %>%
  filter(Variable != '') %>%
  mutate(Variable = ifelse(Variable == '', '', Variable))


sd_df <- data.frame(MiniPredictor1 = 1/sd(OutsideData$TotalPrivateConstructionSpendingManufacturingintheUnitedStates),
                    MiniPredictor2 = 1/sd(CESData$`Expenditures_FloorCoveringsbyIncomeBeforeTaxes_$40_000to$49_999`),

                    SmallPredictor1 = 1/sd(OutsideData$ElectronicsandApplianceStores),
                    SmallPredictor2 = 1/sd(CESData$`Expenditures_HouseholdFurnishingsandEquipmentbyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)`),

                    MediumPredictor1 = 1/sd(CESData$Expenditures_MajorAppliancesbyHousingTenure_HomeOwner),

                    LargePredictor1 = 1/sd(CESData$`Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)`),
                    LargePredictor2 = 1/sd(OutsideData$BuildingMaterialsandSuppliesDealers),
                    LargePredictor3 = 1/sd(BurnsData$`Existing.Home.Median.Price.YOY.%`),

                    PrecipPredictor1 = 1/sd(EastPrecipRCPDiff$May),
                    PrecipPredictor2 = 1/sd(OutsideData$GasolineStations))
sd_df <- sd_df %>%
  pivot_longer(cols = 1:10) %>%
  rename(`1/sd` = value)

cor_df <- data.frame(MiniPredictor1 = Correlations[Correlations$Variable == 'TotalPrivateConstructionSpendingManufacturingintheUnitedStates',]$Mini,
                     MiniPredictor2 = Correlations[Correlations$Variable == 'Expenditures_FloorCoveringsbyIncomeBeforeTaxes_$40_000to$49_999',]$Mini,

                     SmallPredictor1 = Correlations[Correlations$Variable == 'ElectronicsandApplianceStores',]$Small,
                     SmallPredictor2 = Correlations[Correlations$Variable == 'Expenditures_HouseholdFurnishingsandEquipmentbyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)',]$Small,

                     MediumPredictor1 = Correlations[Correlations$Variable == 'Expenditures_MajorAppliancesbyHousingTenure_HomeOwner',]$Medium,

                     LargePredictor1 = Correlations[Correlations$Variable == 'Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)',]$Large,
                     LargePredictor2 = Correlations[Correlations$Variable == 'BuildingMaterialsandSuppliesDealers',]$Large,
                     LargePredictor3 = Correlations[Correlations$Variable == 'Existing.Home.Median.Price.YOY.%',]$Large,

                     PrecipPredictor1 = Correlations[Correlations$Variable == 'May_CR_EastUS_RCP4.5',]$DisRepair,
                     PrecipPredictor2 = Correlations[Correlations$Variable == 'GasolineStations',]$DisRepair)

cor_df <- cor_df %>%
  pivot_longer(cols = 1:10) %>%
  rename(cor = value)

PredProps <- sd_df %>%
  left_join(cor_df, by = 'name')

PredProps <- PredProps %>%
  mutate(Group = str_extract(name, ".*?(?=P|$)"))
PredProps <- PredProps %>%
  mutate(Group = ifelse(name %in% c('PrecipPredictor1', 'PrecipPredictor2'), 'Precip', Group))

PredProps <- PredProps %>%
  group_by(Group) %>%
  mutate(sdProp = `1/sd`/sum(`1/sd`),
         corProp = cor/sum(cor)) %>%
  ungroup()
PredProps <- PredProps %>%
  mutate(FinalProp = (sdProp+corProp)/2)

PredProps$Variable <- c('TotalPrivateConstructionSpendingManufacturingintheUnitedStates',
                        'Expenditures_FloorCoveringsbyIncomeBeforeTaxes_$40_000to$49_999',

                        'ElectronicsandApplianceStores',
                        'Expenditures_HouseholdFurnishingsandEquipmentbyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)',

                        'Expenditures_MajorAppliancesbyHousingTenure_HomeOwner',

                        'Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)',
                        'BuildingMaterialsandSuppliesDealers',
                        'Existing.Home.Median.Price.YOY.%',

                        'May_CR_EastUS_RCP4.5',
                        'GasolineStations')


dataset_list <- list(Correlations, PredProps)


write.xlsx(dataset_list, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_1999_2009.xlsx")

# write.xlsx(AllData, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/AllData.xlsx")
# write.xlsx(OutsideData, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/OutsideData.xlsx")
# write.xlsx(LumberData, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/LumberData.xlsx")
# write.xlsx(CESData, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/CESData.xlsx")
# write.xlsx(Permit_HIRL_Data, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/Permit_HIRL_Data.xlsx")
# write.xlsx(EastPrecipDiff, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/EastPrecipDiff.xlsx")
# write.xlsx(BurnsData, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/OutsideData_fredr/BurnsData.xlsx")

ggplot(AllData, aes(Year, DisRepairDiff)) +
  geom_line() +
  geom_line(data = OutsideData, aes(Date, GasolineStations), color = 'red') +
  geom_line(data = OutsideData, aes(Date, FuelDealers), color = 'green') +
  geom_line(data = OutsideData, aes(Date, ManufacturersNewOrdersConsumerGoods), color = 'orange') +
  geom_line(data = EastPrecipDiff, aes(Year, Apr), color = 'blue') +
  scale_x_continuous(breaks = c(seq(1999,2021,2)), limits = c(2011, 2021)) +
  scale_y_continuous(breaks = c(seq(-.4,1.1,.1))) +
  labs(title = 'Disaster Repair Project Growth Rate vs Possible Predictors',
       subtitle = 'Black = Disaster Repair Project Spend /nRed = Gasoline Station Spending /nBlue = East US April Precipitation /nGreen = Fuel Dealer Spending /nOrange = Manufacturers New Orders: Consumer Goods') +
  ylab('Growth Rate') +
  theme_jbrec()

ggplot(AllData, aes(Year, MiniDiff)) +
  geom_line(size =1) +
  geom_point() +
  geom_line(data = LumberData, aes(Date, ProducerPriceIndexbyIndustryCutStock_ResawingLumber_andPlaningHardwoodCutStockandDimension), color = 'green') +
  geom_line(data = CESData, aes(Date, Expenditures_MajorAppliancesbySizeofConsumerUnit_TwoPeopleinConsumerUnit), color = 'red') +
  geom_line(data = CESData, aes(Date, `Expenditures_MajorAppliancesbyIncomeBeforeTaxes_$30_000to$39_999`), color = 'blue') +
  geom_line(data = CESData, aes(Date, `Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Second20Percent(21stto40thPercentile)`), color = 'orange') +
  scale_x_continuous(breaks = c(seq(1999,2021,2))) +
  scale_y_continuous(breaks = c(seq(-.25,.45,.1))) +
  labs(title = 'Mini Project Growth Rate vs Possible Predictors', subtitle = 'Black = Mini Project Spend /nRed = Major Appliances: 2 Person Households /nGreen = PI for Cut Stock, Resawing Lumber, & Planing Hardwood Cut Stock/Dimension /nOrange = Major Appliances: Second Lowest Income Quintile') +
  ylab('Growth Rate') +
  theme(axis.text = element_text(angle = 90)) +
  theme_jbrec(linewidth = )

ggplot(AllData, aes(Year, SmallDiff)) +
  geom_line(size = 1.5) +
  geom_point() +
  geom_line(data = OutsideData, aes(Date, RetailTrade_ExcludingMotorVehicleandPartsDealers), color = 'green', size = 1.5) +
  geom_line(data = OutsideData, aes(Date, ElectronicShoppingandMail_OrderHouses), color = 'red', size = 1.5) +
  geom_line(data = CESData, aes(Date, `Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)`), color = 'blue', size = 1.5) +
  scale_x_continuous(breaks = c(seq(1999,2021,2))) +
  scale_y_continuous(breaks = c(seq(-.1,.5,.1))) +
  labs(title = 'Mini Project Growth Rate vs Possible Predictors', subtitle = 'Black = Mini Project Spend \nRed = Major Appliances: 2 Person Households \nGreen = PI for Cut Stock, Resawing Lumber, & Planing Hardwood Cut Stock/Dimension \nOrange = Major Appliances: Second Lowest Income Quintile') +
  ylab('Growth Rate') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_jbrec()


ggplot(AllData, aes(Year, MediumDiff)) +
  geom_line(size = 1.5) +
  geom_point() +
  geom_line(data = OutsideData, aes(Date, ManufacturersNewOrdersConstructionMaterialsandSupplies), color = 'green', size = 1.5) +
  geom_line(data = LumberData, aes(Date, `IndustrialProductionManufacturingDurableGoodsMillwork(NAICS=32191)`), color = 'red', size = 1.5) +
  geom_line(data = OutsideData, aes(Date, RetailTrade_ExcludingMotorVehicleandPartsDealers), color = 'orange', size = 1.5) +
  geom_line(data = CESData, aes(Date, `Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)`), color = 'blue', size = 1.5) +
  scale_x_continuous(breaks = c(seq(1999,2021,2))) +
  scale_y_continuous(breaks = c(seq(-.4,.3,.1)), limits= c(-.4, .3)) +
  labs(title = 'Medium Project Growth Rate vs Possible Predictors', subtitle = 'Black = Medium Project Spend\nRed = Industrial Production: Manufacturing Durable Goods (Millwork)\nGreen = Manufacturers New Orders: Construction Materials and Supplies\nOrange = Retail Trade Excluding Car/Parts Dealers\nBlue = Major Appliances: Highest Income Quintile') +
  ylab('Growth Rate') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_jbrec()


ggplot(AllData, aes(Year, LargeDiff)) +
  geom_line(size = 1) +
  geom_point() +
  geom_line(data = OutsideData, aes(Date, NewPrivately_OwnedHousingUnitsUnderConstructionUnitsinBuildingswith2_4Units), color = 'green', size = 1) +
  geom_line(data = LumberData, aes(Date, `ProducerPriceIndexbyIndustryPrefabricatedWoodBuildingManufacturingPrefabricatedStationaryWoodBuildings_Components(NotSoldasCompleteUnits)`), color = 'red', size = 1.5) +
  geom_line(data = OutsideData, aes(Date, BuildingMaterialsandSuppliesDealers), color = 'orange', size = 1) +
  geom_line(data = OutsideData, aes(Date, Furniture_HomeFurnishings_Electronics_andApplianceStores), color = 'blue', size = 1) +
  scale_x_continuous(breaks = c(seq(1999,2021,2))) +
  scale_y_continuous(breaks = c(seq(-.5,.6,.1)), limits = c(-.5, .6)) +
  labs(title = 'Large Project Growth Rate vs Possible Predictors', subtitle = 'Black = Large Project Spend\nRed = PI for Prefab Wood-Building-Manufacturing/Stationary-Wood-Building Components (Not Sold as Complete Units) \nGreen = New Privately Owned Housing Units Under Construction: Units in Buildings with 2-4 Units \nOrange = Building Materials and Supplies Dealers \nBlue = Furniture, Home Furnishings, Electronics, & Appliance Stores') +
  ylab('Growth Rate') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_jbrec()



# NOAAData <- read.csv("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/NOAAData.csv")
# NOAAData$Year <- as.numeric(NOAAData$Year)
# NOAAData <- NOAAData %>%
#   filter(Year >= 1994 & Year <= 2021)
# # Define a 'Years' variable
# Years <- c(seq(1995, 2021, 2))
#
# # Define 'CESVars' to contain all variable names from the CES data (not including 'Year' of course)
# NOAAVars <- names(NOAAData[, 2:9])
# # Establish an empty list, this will be populated below
# NOAAList <- list()
#
# # For each CES Variable and Year combination
# for (x in 1:length(NOAAVars)) {
#   for (i in Years) {
#     # Append to 'NOAAList' the following: (Current Year + Prior Year)/2
#     NOAAList <- append(NOAAList,
#                        (NOAAData[NOAAData$Year == i, ][, NOAAVars[x]] + NOAAData[NOAAData$Year == i-1, ][, NOAAVars[x]]))
#
#   }
# }
# # Convert CESList to a dataframe
# NOAAList <- as.data.frame(NOAAList)
# #Pivot CESList to a long format
# NOAAList <- NOAAList %>%
#   pivot_longer(cols = 1:112)
#
# # Create a template dataframe to place the average 2-year CES values in, nrow = 13 for 13 AHS years (1997-2021), ncol = 28 for 28 CES variables
# NOAA <- data.frame(matrix(NA, nrow = 14, ncol = 8))
# # Transer variable names to CES from CESVars
# colnames(NOAA) <- NOAAVars
#
# # For each number 1-30 (28 = number of CES variables)
# for (i in 1:8) {
#   # Define a start row (1 when i=1, 14 when i=2, etc.)
#   start_row <- (i - 1) * 14 + 1
#   # Define an end row (13 when i = 1, 26 when i=2, etc.)
#   end_row <- i * 14
#   # Paste the values within start_row/end_row into the ith column in CES...This will place each variable's value into a seperate column.
#   NOAA[, i] <- NOAAList[start_row:end_row, 2, drop = FALSE]
# }
#
#
# NOAA$Year <- Years
# # Reorder columns so year is first
# NOAA <- NOAA[, c(9, 1:8)]
# # Convert the Year variable from numeric to a date
# NOAA$Year <- as.Date(paste0(NOAA$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
# NOAA <- NOAA %>%
#   mutate(All = ifelse(All == 0, 0.00001, All))
#
# # Create an xts (timeseries) dataset using ARTS
# NOAA <- as.xts(NOAA)
#
#
# # Calculate the growth rate manually using diff
# NOAADiff <- diff(NOAA) / dplyr::lag(NOAA, n = 1)
# NOAADiff <- as.data.frame(NOAADiff$All)
# NOAADiff$Year <- Years
# NOAADiff <- NOAADiff %>%
#   filter(Year >= 1997)
# NOAA$Year <- Years
#
# NOAADiff <- NOAADiff %>%
#   filter(Year >= 1999)
#
# NOAAMiniCor <- as.data.frame(cor(AllData$MiniDiff, NOAADiff[, c(1)]))
# NOAAMiniCor <- NOAAMiniCor %>%
#   pivot_longer(cols = c(1))
#
# NOAASmallCor <- as.data.frame(cor(AllData$SmallDiff, NOAADiff[, c(1)]))
# NOAASmallCor <- NOAASmallCor %>%
#   pivot_longer(cols = c(1))
#
# NOAAMediumCor <- as.data.frame(cor(AllData$MediumDiff, NOAADiff[, c(1)]))
# NOAAMediumCor <- NOAAMediumCor %>%
#   pivot_longer(cols = c(1))
#
# NOAALargeCor <- as.data.frame(cor(AllData$LargeDiff, NOAADiff[, c(1)]))
# NOAALargeCor <- NOAALargeCor %>%
#   pivot_longer(cols = c(1))
#
# NOAADisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, NOAADiff[, c(1)]))
# NOAADisRepairCor <- NOAADisRepairCor %>%
#   pivot_longer(cols = c(1))
#
# # Condense all correlations into one dataframe called 'ARTS_Correlations'
# NOAA_Correlations <- data.frame(Variable = 'BillionDollarStormSpending',
#                                 Mini = NOAAMiniCor$value,
#                                 Small = NOAASmallCor$value,
#                                 Medium = NOAAMediumCor$value,
#                                 Large = NOAALargeCor$value,
#                                 DisRepair = NOAADisRepairCor$value)
