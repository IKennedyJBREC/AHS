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
library(fredr)
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

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

# Joi the inflation data to the ARTS data
ARTSData <- ARTSData %>%
  left_join(Inflation, by = 'Date')

ARTSCols <- as.numeric(ncol(ARTSData)-1)
# Inflation adjust the ARTS data
ARTSData[,2:ARTSCols] <- ARTSData[,2:ARTSCols]*ARTSData$Rate

# Drop the inflation rate
ARTSData <- ARTSData %>%
  select(-Rate)

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

OtherDataCols <- as.numeric(ncol(OtherData)-1)
OtherData[,2:OtherDataCols] <- OtherData[,2:OtherDataCols]*OtherData$Rate

OtherData <- OtherData %>%
  select(-Rate)


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

LumberDataCols <- as.numeric(ncol(LumberData)-1)
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

LumberVariables <- names(LumberData[-1])
LumberData <- LumberData %>%
  filter(Date >= 1994)

# Read in Burns Data
BurnsData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/Burns_Data_Download.xlsx")
BurnsVariables <- names(BurnsData[-1])


# Read in Permit Data
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

CESDataCols <- as.numeric(ncol(CESData)-1)
CESData[,2:CESDataCols] <- CESData[,2:CESDataCols]*CESData$Rate

CESData <- CESData %>%
  select(-Rate)

CESDataVariables <- names(CESData[-1])
CESData <- CESData %>%
  filter(Date >= 1994)


rm(ARTSData, ARTSVariables, BurnsDataList, BurnsDataTemplate, BurnsVariables, ConstructionData, ConstructionVariables, HIRLData, PermitData, dataframes_list, LumberDataList,
   data, LumberDataTemplate, LumberVariables, yearly_sum, yearly_sums, OutsideDataTemplate, OutsideDataList, ARTSDates, ConstructionDates, LumberDates, OtherDates, OtherVariables,
   OtherData, PermitDates, Permit_HIRL_DataList, Permit_HIRL_Template, ApplianceVariables, ApplianceVariables2, ApplianceVariables3, FurnitureVariables, FurnitureVariables2,
   MaintenanceVariables, MaintenanceVariables2, FloorCoveringVariables, ARTSCols, CESDataCols, CESDataVariables, CESDates, col, col_name, endpoints, CPI2021, HIRLDates, i,
   LumberDataCols, OtherDataCols, OutsideDataVariables, Permit_HIRL_Variables)

PredictorData <- OutsideData %>%
  left_join(CESData, by = 'Date')
PredictorData <- PredictorData %>%
  left_join(LumberData, by = 'Date')


###

# Read in total precipitation (US East) data
EastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation.xlsx")
EastPrecipData <- EastPrecipData %>%
  rename(Date = Year)

names(EastPrecipData) <- paste0(names(EastPrecipData), '_CR_EastUS')
EastPrecipData <- EastPrecipData %>%
  rename(Date = Date_CR_EastUS)

# Read in total precipitation (US Northeast) data
NortheastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USNortheast_TotalPrecipitation.xlsx")
NortheastPrecipData <- NortheastPrecipData %>%
  rename(Date = Year)
names(NortheastPrecipData) <- paste0(names(NortheastPrecipData), '_CR_NEUS')
NortheastPrecipData <- NortheastPrecipData %>%
  rename(Date = Date_CR_NEUS)

# Read in total precipitation (US Northeast) data
EastPrecip_RCP4.5 <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation_RCP4.5.xlsx")
EastPrecip_RCP4.5 <- EastPrecip_RCP4.5 %>%
  rename(Date = Year)
names(EastPrecip_RCP4.5) <- paste0(names(EastPrecip_RCP4.5), '_CR_EastUS_RCP4.5')
EastPrecip_RCP4.5 <- EastPrecip_RCP4.5 %>%
  rename(Date = Date_CR_EastUS_RCP4.5)

Precip_Data <- EastPrecipData %>%
  left_join(NortheastPrecipData, by = 'Date')
Precip_Data <- Precip_Data %>%
  left_join(EastPrecip_RCP4.5, by = 'Date')

PredictorData <- PredictorData %>%
  left_join(Precip_Data, by = 'Date')

# Read in the AHS 2-year summary data
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx")
# Drop the Aggregate Data (i.e. CostClass == 'All')
Data <- Data %>%
  select(Year, CostClass, TotalJobCost)

# Create seperate dataframes for each CostClass and Disaster Repairs. Each dataframe will have an amended 'TotalJobCost' column name
Mini <- Data %>%
  filter(CostClass == 'Mini') %>%
  select(-CostClass) %>%
  rename(TotalJobCost_Mini = TotalJobCost)
Small <- Data %>%
  filter(CostClass == 'Small') %>%
  select(-CostClass) %>%
  rename(TotalJobCost_Small = TotalJobCost)
Medium <- Data %>%
  filter(CostClass == 'Medium') %>%
  select(-CostClass) %>%
  rename(TotalJobCost_Medium = TotalJobCost)
Large <- Data %>%
  filter(CostClass == 'Large') %>%
  select(-CostClass) %>%
  rename(TotalJobCost_Large = TotalJobCost)
DisRepairs <- Data %>%
  filter(CostClass == 'DisRepairs') %>%
  select(-CostClass) %>%
  rename(TotalJobCost_DisRepairs = TotalJobCost)

# Create an empty template dataframe, where Year is equal to 1994-2021 and all other columns are NA (thses will be filled soon!)
Template <- data.frame(Year = c(seq(1994,2021,1)),
                       Mini = NA,
                       Small = NA,
                       Medium = NA,
                       Large = NA,
                       DisRepairs = NA)

PredProps <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_2010_Current.xlsx", sheet = 2)
PredProps <- PredProps %>%
  select(name, Variable, FinalProp)

# Create a 'MiniPredictor' column in the template dataframe containing the variable to use for Mini (MajorAppliances_40), then join the 2-Year Mini data to the template by year
## Do the same for each of the other cost classes. For Disaster Repairs paste both of the predictor columns
Template$MiniPredictor1 <- PredictorData$Expenditures_MajorAppliancesbySizeofConsumerUnit_TwoPeopleinConsumerUnit
Template$MiniPredictor2 <- PredictorData$`Expenditures_FurniturebyQuintilesofIncomeBeforeTaxes_Third20Percent(41stto60thPercentile)`
Template$MiniPredictor3 <- PredictorData$`Expenditures_SmallAppliances_MiscellaneousHousewaresbyQuintilesofIncomeBeforeTaxes_Second20Percent(21stto40thPercentile)`
Template <- Template %>%
  left_join(Mini, by = 'Year')

Template$SmallPredictor1 <- PredictorData$`Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)`
Template$SmallPredictor2 <- PredictorData$`Expenditures_FurniturebyQuintilesofIncomeBeforeTaxes_Third20Percent(41stto60thPercentile)`

Template <- Template %>%
  left_join(Small, by = 'Year')

Template$MediumPredictor1 <- PredictorData$Expenditures_MajorAppliancesbySizeofConsumerUnit_FiveorMorePeopleinConsumerUnit
Template$MediumPredictor2 <- PredictorData$`Expenditures_FurniturebyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)`
Template$MediumPredictor3 <- PredictorData$`Expenditures_FurniturebyQuintilesofIncomeBeforeTaxes_Third20Percent(41stto60thPercentile)`
Template$MediumPredictor4 <- PredictorData$`Expenditures_MajorAppliancesbyQuintilesofIncomeBeforeTaxes_Highest20Percent(81stto100thPercentile)`
Template <- Template %>%
  left_join(Medium, by = 'Year')

Template$LargePredictor1 <- PredictorData$`Expenditures_FurniturebyQuintilesofIncomeBeforeTaxes_Fourth20Percent(61stto80thPercentile)`
Template$LargePredictor2 <- PredictorData$BuildingMaterialsandSuppliesDealers
Template <- Template %>%
  left_join(Large, by = 'Year')

Template$PrecipPredictor1 <- PredictorData$AMJ_CR_NEUS
Template$PrecipPredictor2 <- PredictorData$GasolineStations
Template <- Template %>%
  left_join(DisRepairs, by = 'Year')



MiniPredictor1_Mult <- PredProps[PredProps$name == 'MiniPredictor1',]$FinalProp
MiniPredictor2_Mult <- PredProps[PredProps$name == 'MiniPredictor2',]$FinalProp
MiniPredictor3_Mult <- PredProps[PredProps$name == 'MiniPredictor3',]$FinalProp

SmallPredictor1_Mult <- PredProps[PredProps$name == 'SmallPredictor1',]$FinalProp
SmallPredictor2_Mult <- PredProps[PredProps$name == 'SmallPredictor2',]$FinalProp

MediumPredictor1_Mult <- PredProps[PredProps$name == 'MediumPredictor1',]$FinalProp
MediumPredictor2_Mult <- PredProps[PredProps$name == 'MediumPredictor2',]$FinalProp
MediumPredictor3_Mult <- PredProps[PredProps$name == 'MediumPredictor3',]$FinalProp
MediumPredictor4_Mult <- PredProps[PredProps$name == 'MediumPredictor4',]$FinalProp

LargePredictor1_Mult <- PredProps[PredProps$name == 'LargePredictor1',]$FinalProp
LargePredictor2_Mult <- PredProps[PredProps$name == 'LargePredictor2',]$FinalProp

PrecipPredictor1_Mult <- PredProps[PredProps$name == 'PrecipPredictor1',]$FinalProp
PrecipPredictor2_Mult <- PredProps[PredProps$name == 'PrecipPredictor2',]$FinalProp



# For each year from 1994-current
for (i in c(seq(1994,2021,1))){
  # If the year is even (i.e. an not an AHS Survey YEar)
  if (i %% 2 == 0){
    # Find the proportion of the MiniPredictor that falls within each of the bordering years (i.e. proportion of MiniPredictor that falls in 1994 vs 1995, a fraction)
    ## Multiply the proportion by the 2-Year total to find 1-year total for the given year. Paste that value into the 'Mini' column
    Template[Template$Year == i, ]$Mini <-

      ((Template[Template$Year == i+1, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor1/(Template[Template$Year == i, ]$MiniPredictor1 + Template[Template$Year == i+1, ]$MiniPredictor1)) * MiniPredictor1_Mult)) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor2/(Template[Template$Year == i, ]$MiniPredictor2 + Template[Template$Year == i+1, ]$MiniPredictor2))) * MiniPredictor2_Mult) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor3/(Template[Template$Year == i, ]$MiniPredictor3 + Template[Template$Year == i+1, ]$MiniPredictor3))) * MiniPredictor3_Mult)

    # Do the same for other CostClasses when an even year
    Template[Template$Year == i, ]$Small <-

      ((Template[Template$Year == i+1, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor1/(Template[Template$Year == i, ]$SmallPredictor1 + Template[Template$Year == i+1, ]$SmallPredictor1)) * SmallPredictor1_Mult)) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor2/(Template[Template$Year == i, ]$SmallPredictor2 + Template[Template$Year == i+1, ]$SmallPredictor2))) * SmallPredictor2_Mult)


    Template[Template$Year == i, ]$Medium <-

      ((Template[Template$Year == i+1, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor1/(Template[Template$Year == i, ]$MediumPredictor1 + Template[Template$Year == i+1, ]$MediumPredictor1)) * MediumPredictor1_Mult)) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor2/(Template[Template$Year == i, ]$MediumPredictor2 + Template[Template$Year == i+1, ]$MediumPredictor2))) * MediumPredictor2_Mult) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor3/(Template[Template$Year == i, ]$MediumPredictor3 + Template[Template$Year == i+1, ]$MediumPredictor3))) * MediumPredictor3_Mult) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor4/(Template[Template$Year == i, ]$MediumPredictor4 + Template[Template$Year == i+1, ]$MediumPredictor4))) * MediumPredictor4_Mult)


    Template[Template$Year == i, ]$Large <-

      ((Template[Template$Year == i+1, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor1/(Template[Template$Year == i, ]$LargePredictor1 + Template[Template$Year == i+1, ]$LargePredictor1)) * LargePredictor1_Mult)) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor2/(Template[Template$Year == i, ]$LargePredictor2 + Template[Template$Year == i+1, ]$LargePredictor2))) * LargePredictor2_Mult)


    Template[Template$Year == i, ]$DisRepairs <-

      ((Template[Template$Year == i+1, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$PrecipPredictor1/(Template[Template$Year == i, ]$PrecipPredictor1 + Template[Template$Year == i+1, ]$PrecipPredictor1)) * PrecipPredictor1_Mult)) +

      ((Template[Template$Year == i+1, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$PrecipPredictor2/(Template[Template$Year == i, ]$PrecipPredictor2 + Template[Template$Year == i+1, ]$PrecipPredictor2))) * PrecipPredictor2_Mult)
  }

  # If the year is odd
  else{

    # Find the predictor-proportioned values by using the prior year rather than the year ahead (i.e. if i = 1995, we want to use 1994 vs 1995 not 1995 vs 1996)

    Template[Template$Year == i, ]$Mini <-
       (
          (Template[Template$Year == i, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor1/(Template[Template$Year == i, ]$MiniPredictor1 + Template[Template$Year == i-1, ]$MiniPredictor1)) * MiniPredictor1_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor2/(Template[Template$Year == i, ]$MiniPredictor2 + Template[Template$Year == i-1, ]$MiniPredictor2)) * MiniPredictor2_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor3/(Template[Template$Year == i, ]$MiniPredictor3 + Template[Template$Year == i-1, ]$MiniPredictor3)) * MiniPredictor3_Mult)
       )


    Template[Template$Year == i, ]$Small <-
      (
          (Template[Template$Year == i, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor1/(Template[Template$Year == i, ]$SmallPredictor1 + Template[Template$Year == i-1, ]$SmallPredictor1)) * SmallPredictor1_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor2/(Template[Template$Year == i, ]$SmallPredictor2 + Template[Template$Year == i-1, ]$SmallPredictor2)) * SmallPredictor2_Mult)
      )


    Template[Template$Year == i, ]$Medium <-
      (
          (Template[Template$Year == i, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor1/(Template[Template$Year == i, ]$MediumPredictor1 + Template[Template$Year == i-1, ]$MediumPredictor1)) * MediumPredictor1_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor2/(Template[Template$Year == i, ]$MediumPredictor2 + Template[Template$Year == i-1, ]$MediumPredictor2)) * MediumPredictor2_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor3/(Template[Template$Year == i, ]$MediumPredictor3 + Template[Template$Year == i-1, ]$MediumPredictor3)) * MediumPredictor3_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor4/(Template[Template$Year == i, ]$MediumPredictor4 + Template[Template$Year == i-1, ]$MediumPredictor4)) * MediumPredictor4_Mult)
      )

    Template[Template$Year == i, ]$Large <-
      (
          (Template[Template$Year == i, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor1/(Template[Template$Year == i, ]$LargePredictor1 + Template[Template$Year == i-1, ]$LargePredictor1)) * LargePredictor1_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor2/(Template[Template$Year == i, ]$LargePredictor2 + Template[Template$Year == i-1, ]$LargePredictor2)) * LargePredictor2_Mult)
      )

    # Do the same for each of the disaster repair predictor columns, though use the multipliers to find the appropriate value (i.e. FuelDealers 1-Year Total X FDMult + NEPrecip 1-Year Total X PrecipMult)
    Template[Template$Year == i, ]$DisRepairs <-
      (
          (Template[Template$Year == i, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$PrecipPredictor1/(Template[Template$Year == i, ]$PrecipPredictor1 + Template[Template$Year == i-1, ]$PrecipPredictor1)) * PrecipPredictor1_Mult) +

          (Template[Template$Year == i, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$PrecipPredictor2/(Template[Template$Year == i, ]$PrecipPredictor2 + Template[Template$Year == i-1, ]$PrecipPredictor2)) * PrecipPredictor2_Mult)
      )
  }

}

# Select relevant columns
Template <- Template %>%
  select(Year:DisRepairs)
# Re-read in the 2-year summary data
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx")

# Select relevant columns from the 2-year summary
Data <- Data %>%
  select(Year:TotalJobCost)

# Pivot the data to a long format with CostClasses going to the 'CostClass' column, and 1-year total costs to 'TotalJobCost_New'
Template <- Template %>%
  pivot_longer(cols = 2:6, names_to = 'CostClass', values_to = 'TotalJobCost_New')

# Join the 2-Year summary data tot the 1-Year breakout data by year and CostClass
Template <- Template %>%
  left_join(Data, by = c('Year', 'CostClass'))

# Define the desired order of CostClass
order_levels <- c("Mini", "Small", "Medium", "Large", "DisRepairs")

# Convert CostClass to a factor with the desired order
Template$CostClass <- factor(Template$CostClass, levels = order_levels)
legend_order <- rev(unique(Template$CostClass))

OldTemplate <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/OneYearApprox.xlsx")

OldTemplate %>%
  filter(Year >= 2010) %>%
  ggplot(aes(x = Year, y = TotalJobCost_New/1000000000, fill = fct_rev(CostClass))) +
  scale_y_continuous(labels = function(income) format(income, big.mark = ",", scientific = FALSE), breaks = c(seq(0,600,50)), limits = c(0,600)) +
  # percent_format()) +
  # function(income) format(income, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = c(seq(2010, 2021, 1))) +
  geom_col(color = NA) +
  scale_fill_jbrec(discrete = TRUE, palette = 'PrimaryAndSecondary') +
  geom_text(aes(label = scales::comma(round(TotalJobCost_New/1000000000)), y = TotalJobCost_New/1000000000),
            position = position_stack(vjust = .5), color = 'black', size = 4) +
  guides(fill = guide_legend(reverse = T)) +
  labs(
    x = "Year",
    y = "Total Spend",
    title = "Total Repair/Remodel Spend by Project Size",
    caption = "Source: John Burns Research and Consulting tabulations of American Housing Survey.",
    subtitle = 'Billions of USD (Real)'
  ) +
  theme_jbrec(textcolor = 'black', backgroundcolor = 'white') +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90))


Template %>%
  filter(Year >= 2010) %>%
  ggplot(aes(x = Year, y = TotalJobCost_New/1000000000, fill = CostClass)) +
  scale_y_continuous(labels = function(income) format(income, big.mark = ",", scientific = FALSE), breaks = c(seq(0,600,50))) +
  # percent_format()) +
  # function(income) format(income, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = c(seq(1994, 2021, 1))) +
  geom_col(color = NA) +
  scale_fill_jbrec(discrete = TRUE, palette = 'PrimaryAndSecondary') +
  geom_text(aes(label = scales::comma(round(TotalJobCost_New/1000000000)), y = TotalJobCost_New/1000000000),
            position = position_stack(vjust = .5), color = 'black', size = 4) +
  guides(fill = guide_legend(reverse = T)) +
  labs(
    x = "Year",
    y = "Total Spend",
    title = "Total Repair/Remodel Spend by Project Size",
    caption = "Source: John Burns Research and Consulting tabulations of American Housing Survey.",
    subtitle = 'Billions of USD (Real)'
  ) +
  theme_jbrec(textcolor = 'black', backgroundcolor = 'white') +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90))
  facet_wrap(~CostClass, scales = 'free', nrow = 4)

Template <- Template %>%
  filter(Year >= 2010)
write.xlsx(Template, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/OneYearApprox_2010_Current.xlsx")
