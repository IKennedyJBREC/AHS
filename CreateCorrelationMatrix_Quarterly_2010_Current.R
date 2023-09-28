library(tidyverse)
library(fredr)
library(openxlsx)
library(lubridate)
library(xts)
library(blsR)

fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')
bls_set_key('55c1f6ba5ffb44bfbd4d2eb29af72dab')

conflicted::conflicts_prefer(dplyr::lag, dplyr::filter)

# Function to calculate percentage difference
percentage_diff <- function(x) {
  diff_x <- c(NA, diff(x))
  percentage_diff_x <- (diff_x / lag(x, n = 1))
  return(percentage_diff_x)
}

# ARTSVariables <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 4)
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

# Apply the percentage_diff function to all columns
ARTSData <- as.data.frame(lapply(ARTSData[,-1], percentage_diff))

# Reattach dates to ARTSData
ARTSData$Date <- ARTSDates
# Reorder ARTSData so that Date is the first column
ARTSData <- ARTSData[,c(ARTSCols, 1:ARTSCols-1)]
# Filter for data from 1995-current
ARTSData <- ARTSData %>%
  filter(Date >= '1995-01-01') %>%
  mutate(Date = year(Date))

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

# Apply the percentage_diff function to all columns
OtherData <- as.data.frame(lapply(OtherData[,-1], percentage_diff))

OtherData$Date <- OtherDates
OtherData <- OtherData[,c(OtherDataCols, 1:OtherDataCols-1)]
OtherData <- OtherData %>%
  filter(Date >= '1995-01-01') %>%
  mutate(Date = year(Date))



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

ConstructionDates <- ConstructionData$Date

# Apply the percentage_diff function to all columns
ConstructionData <- as.data.frame(lapply(ConstructionData[,-1], percentage_diff))

ConstructionData$Date <- ConstructionDates
ConstructionData <- ConstructionData[,c(ncol(ConstructionData), 1:ncol(ConstructionData)-1)]

ConstructionData <- ConstructionData %>%
  filter(Date >= 1995)



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

# Apply the percentage_diff function to all columns
LumberData <- as.data.frame(lapply(LumberData[,-1], percentage_diff))

LumberData$Date <- LumberDates
LumberData <- LumberData[,c(LumberDataCols, 1:LumberDataCols-1)]
LumberData <- LumberData %>%
  filter(Date >= '1995-01-01') %>%
  mutate(Date = year(Date))

OutsideData <- ARTSData %>%
  left_join(ConstructionData, by = 'Date')
OutsideData <- OutsideData %>%
  left_join(OtherData, by = 'Date')
OutsideData <- OutsideData %>%
  left_join(LumberData, by = 'Date')

BurnsData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/Burns_Data_Download.xlsx")
OutsideData <- OutsideData %>%
  left_join(BurnsData, by = 'Date')


EmploymentIDs <- list(Construction = 'CEU2000000002',
                      FramingContractors = 'CEU2023813002',
                      MasonryContractors = 'CEU2023814002',
                      RoofingContractors = 'CEU2023816002',
                      FlooringContractors = 'CEU2023833002',
                      SidingContractors = 'CEU2023817002',
                      ResidentialRemodelers = 'CEU2023611802',
                      FinishCarpentryContractors = 'CEU2023835002',
                      SitePrepContractors = 'CEU2023891002',
                      BldgFinishingContractors = 'CEU2023830002',
                      BldgEqpmtContractors = 'CEU2023820002',
                      SpecialtyTradeContractors = 'CEU2023800002',
                      UtilitySystemContractors = 'CEU2023710002',
                      NonResBldgConstruction = 'CEU2023620002',
                      ResBldgConstruction = 'CEU2023610002',
                      IndBldgConstruction = 'CEU2023621002',
                      BldgConstruction = 'CEU2023600002',
                      OtherBldgFinishingContractors = 'CEU2023839002',
                      OtherSpecialtyTradeContractors = 'CEU2023890002',
                      DrywallInsulationContractors = 'CEU2023831002',
                      GlassGlazingContractors = 'CEU2023815002',
                      OtherBldgEqpmtContractors = 'CEU2023829002',
                      TileTerrazoContractors = 'CEU2023834002',
                      AllOtherSpecialtyTradeContractors = 'CEU2023899002',
                      PaintingWallCoveringContractors = 'CEU2023832002',
                      CommercialInstitutionalBldgConstruction = 'CEU2023622002',
                      PouredConcreteFoundationStructureContractors = 'CEU2023811002',
                      PLumbingHVACContractors = 'CEU2023822002',
                      FoundationStructureBldgExteriorContractors = 'CEU2023810002',
                      StructuralSteelPrecastConcreteContractors = 'CEU2023812002',
                      OtherFoundationStructureBldgExteriorContractors = 'CEU2023819002',
                      ElectricalOtherWiringInstallationContractors = 'CEU2023821002',
                      WaterSewerLineRelatedStructureConstruction = 'CEU2023711002',
                      NewSFHousingConstruction_NoForSaleBuilders = 'CEU2023611502')

EmploymentData <- get_series_tables(series_ids = EmploymentIDs,
                                    api_key = '55c1f6ba5ffb44bfbd4d2eb29af72dab',
                                    start_year = 2006,
                                    end_year = 2021)

final_df <- data.frame()
df_list <- list()

for (name in names(EmploymentData)) {
  df <- as.data.frame(EmploymentData[[name]]) %>%
    mutate(period = gsub('M', '', period),
           date = as.Date(paste0(year, '-', period, '-01'))) %>%
    select(date, value) %>%
    as.xts() %>%
    apply.yearly(FUN = mean) %>%
    as.data.frame() %>%
    mutate(Year = c(seq(2006, 2021, 1)))

  # Store the dataframe in the list
  df_list[[name]] <- df
}

# Bind all dataframes in the list row-wise (by 'Year')
final_df <- bind_rows(df_list, .id = "name")

EmploymentData <- final_df %>%
  pivot_wider(names_from = name, values_from = value)

EmploymentData <- lapply(EmploymentData[,-1], percentage_diff)
EmploymentData <- as.data.frame(EmploymentData)
EmploymentData <- EmploymentData[-1,]
EmploymentData$Year <- c(seq(2007,2021,1))
EmploymentData <- EmploymentData[,c(35, 1:34)]

OutsideData <- OutsideData %>%
  left_join(EmploymentData, by = c('Date' = 'Year'))

OutsideData <- OutsideData %>%
  filter(Date > 2009)



Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/OneYearApprox.xlsx")
Data <- Data %>%
  select(Year, CostClass, TotalJobCost_New)

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
Years <- c(seq(1994, 2021, 1))


# Create 'AllData' containing the TotalJobCost for each of the Cost Classes + DisRepairs/Maintanence
AllData <- data.frame(Year = Mini$Year,
                      Mini = Mini$TotalJobCost_New,
                      Small = Small$TotalJobCost_New,
                      Medium = Medium$TotalJobCost_New,
                      Large = Large$TotalJobCost_New,
                      DisRepair = DisRepair$TotalJobCost_New)

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

AllData <- AllData %>%
  filter(Year > 2009)

OutsideDataCols <- as.numeric(ncol(OutsideData))
# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, OutsideData[, c(2:OutsideDataCols)]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:OutsideDataCols-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, OutsideData[, c(2:OutsideDataCols)]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:OutsideDataCols-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, OutsideData[, c(2:OutsideDataCols)]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:OutsideDataCols-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, OutsideData[, c(2:OutsideDataCols)]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:OutsideDataCols-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, OutsideData[, c(2:OutsideDataCols)]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:OutsideDataCols-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
Correlations <- data.frame(Variable = MiniCor$name,
                           Mini = MiniCor$value,
                           Small = SmallCor$value,
                           Medium = MediumCor$value,
                           Large = LargeCor$value,
                           DisRepair = DisRepairCor$value)


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

# Apply the percentage_diff function to all columns
PermitData <- as.data.frame(lapply(PermitData[,-1], percentage_diff))
PermitData$Date <- PermitDates
PermitData <- PermitData[, c(ncol(PermitData), 1:ncol(PermitData)-1)]
PermitData <- PermitData %>%
  filter(Date > 2009 & Date <= 2021)

PermitDataCols <- as.numeric(ncol(PermitData))
# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, PermitData[, c(2:PermitDataCols)]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:PermitDataCols-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, PermitData[, c(2:PermitDataCols)]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:PermitDataCols-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, PermitData[, c(2:PermitDataCols)]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:PermitDataCols-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, PermitData[, c(2:PermitDataCols)]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:PermitDataCols-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, PermitData[, c(2:PermitDataCols)]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:PermitDataCols-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
PermitCorrelations <- data.frame(Variable = MiniCor$name,
                                 Mini = MiniCor$value,
                                 Small = SmallCor$value,
                                 Medium = MediumCor$value,
                                 Large = LargeCor$value,
                                 DisRepair = DisRepairCor$value)

Correlations <- Correlations %>%
  rbind(PermitCorrelations)


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

HIRLCols <- as.numeric(ncol(HIRLData))
HIRLData <- HIRLData[, c(HIRLCols, 1:HIRLCols-1)]

# Apply the percentage_diff function to all columns
HIRLData <- as.data.frame(lapply(HIRLData[,-1], percentage_diff))
HIRLData$Date <- HIRLDates
HIRLData <- HIRLData[, c(HIRLCols, 1:HIRLCols-1)]
HIRLData <- HIRLData %>%
  filter(Date > 2009 & Date <= 2021)

names(HIRLData) <- paste0(names(HIRLData), '_RR')

# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, HIRLData[, c(2:HIRLCols)]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, HIRLData[, c(2:HIRLCols)]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, HIRLData[, c(2:HIRLCols)]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, HIRLData[, c(2:HIRLCols)]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, HIRLData[, c(2:HIRLCols)]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
HIRLCorrelations1 <- data.frame(Variable = MiniCor$name,
                                Mini = MiniCor$value,
                                Small = SmallCor$value,
                                Medium = MediumCor$value,
                                Large = LargeCor$value,
                                DisRepair = DisRepairCor$value)

Correlations <- Correlations %>%
  rbind(HIRLCorrelations1)

HIRL_Data_RR <- HIRLData

HIRLData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/HIRLData.xlsx", sheet = 2)
HIRLDates <- unique(HIRLData$Year)
HIRLData <- HIRLData %>%
  mutate(Date = paste0(Year, 'Q', Quarter),
         Date = as.yearqtr(Date)) %>%
  select(-c(Year,Quarter))


HIRLData <- as.xts(HIRLData)
endpoints <- endpoints(HIRLData,on = 'years')

HIRLData1 <- period.apply(HIRLData[,1], INDEX = endpoints, FUN = sum)

#Create an empty list to store the yearly sums for each column
yearly_sums <- list()

# Loop through each column of HIRLData and calculate the yearly sum
for (col in 1:ncol(HIRLData)) {
  yearly_sum <- period.apply(HIRLData[, col], INDEX = endpoints, FUN = sum)
  yearly_sums[[col]] <- yearly_sum
}
HIRLData <- as.data.frame(yearly_sums)
HIRLData$Date <- HIRLDates
HIRLCols <- as.numeric(ncol(HIRLData))
HIRLData <- HIRLData[, c(HIRLCols, 1:HIRLCols-1)]

# Apply the percentage_diff function to all columns
HIRLData <- as.data.frame(lapply(HIRLData[,-1], percentage_diff))
HIRLData$Date <- HIRLDates
HIRLData <- HIRLData[, c(HIRLCols, 1:HIRLCols-1)]
HIRLData <- HIRLData %>%
  filter(Date > 2009 & Date <= 2021)

names(HIRLData) <- paste0(names(HIRLData), '_Total')

# Create ARTS[CostClass]Cor for each of the cost classes mentioned above
# Take the correlation between the normalized Total Job Cost and each of the normalized columns in ARTS, convert to a dataframe, and pivot the dataframe to a long format
MiniCor <- as.data.frame(cor(AllData$MiniDiff, HIRLData[, c(2:HIRLCols)]))
MiniCor <- MiniCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

SmallCor <- as.data.frame(cor(AllData$SmallDiff, HIRLData[, c(2:HIRLCols)]))
SmallCor <- SmallCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

MediumCor <- as.data.frame(cor(AllData$MediumDiff, HIRLData[, c(2:HIRLCols)]))
MediumCor <- MediumCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

LargeCor <- as.data.frame(cor(AllData$LargeDiff, HIRLData[, c(2:HIRLCols)]))
LargeCor <- LargeCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

DisRepairCor <- as.data.frame(cor(AllData$DisRepairDiff, HIRLData[, c(2:HIRLCols)]))
DisRepairCor <- DisRepairCor %>%
  pivot_longer(cols = c(1:HIRLCols-1))

# Condense all correlations into one dataframe called 'ARTS_Correlations'
HIRLCorrelations2 <- data.frame(Variable = MiniCor$name,
                                Mini = MiniCor$value,
                                Small = SmallCor$value,
                                Medium = MediumCor$value,
                                Large = LargeCor$value,
                                DisRepair = DisRepairCor$value)

Correlations <- Correlations %>%
  rbind(HIRLCorrelations2)

HIRL_Data_Total <- HIRLData

# Read in total precipitation (US East) data
EastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation.xlsx")
# Store variable names in 'EastPrecipVars'
EastPrecipVars <- names(EastPrecipData[, 2:ncol(EastPrecipData)])

EastPrecipData$Year <- as.Date(paste0(EastPrecipData$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecipData <- as.xts(EastPrecipData)

# Calculate the growth rate manually using diff
EastPrecipDiff <- diff(EastPrecipData) / dplyr::lag(EastPrecipData, n = 1)
EastPrecipDiff <- as.data.frame(EastPrecipDiff)
EastPrecipDiff$Year <- c(seq(1994, 2021, 1))
EastPrecipDiff <- EastPrecipDiff[, c(length(EastPrecipVars)+1, 1:length(EastPrecipVars))]
EastPrecipDiff <- EastPrecipDiff %>%
  filter(Year > 2009 & Year <= 2021)

# Create EastPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question
EastPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, EastPrecipDiff[, c(2:ncol(EastPrecipDiff))]))

# Pivot PrecipCor to a long format
EastPrecipCor <- EastPrecipCor %>%
  pivot_longer(cols = c(1:ncol(EastPrecipCor)))


# Read in total precipitation (US Northeast) data
NortheastPrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USNortheast_TotalPrecipitation.xlsx")
# Store variable names in 'NEPrecipVars'
NEPrecipVars <- names(NortheastPrecipData[, 2:ncol(NortheastPrecipData)])

NortheastPrecipData$Year <- as.Date(paste0(NortheastPrecipData$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
NortheastPrecipData <- as.xts(NortheastPrecipData)

# Calculate the growth rate manually using diff
NEPrecipDiff <- diff(NortheastPrecipData) / dplyr::lag(NortheastPrecipData, n = 1)
NEPrecipDiff <- as.data.frame(NEPrecipDiff)
NEPrecipDiff$Year <- c(seq(1994, 2021, 1))
NEPrecipDiff <- NEPrecipDiff[, c(ncol(NEPrecipDiff), 1:ncol(NEPrecipDiff)-1)]
NEPrecipDiff <- NEPrecipDiff %>%
  filter(Year > 2009 & Year <= 2021)


# Create NEPrecipCor, containing the correlation values between DisRepair_Normalized and the precipitation variable in question
NEPrecipCor <- as.data.frame(cor(AllData$DisRepairDiff, NEPrecipDiff[, c(2:ncol(NEPrecipDiff))]))

# Pivot NEPrecipCor to a long format
NEPrecipCor <- NEPrecipCor %>%
  pivot_longer(cols = c(1:ncol(NEPrecipCor)))

# Read in total precipitation (US Northeast) data
EastPrecip_RCP4.5 <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation_RCP4.5.xlsx")
EastPrecip_RCP4.5 <- EastPrecip_RCP4.5 %>%
  filter(Year >= 1994 & Year <= 2021)
EastPrecip_RCP4.5$Year <- as.Date(paste0(EastPrecip_RCP4.5$Year, "-01-01", sep = ""), format = "%Y-%m-%d")
EastPrecip_RCP4.5 <- as.xts(EastPrecip_RCP4.5)

# Calculate the growth rate manually using diff
EastPrecipRCPDiff <- diff(EastPrecip_RCP4.5) / dplyr::lag(EastPrecip_RCP4.5, n = 1)
EastPrecipRCPDiff <- as.data.frame(EastPrecipRCPDiff)
EastPrecipRCPDiff$Year <- c(seq(1994, 2021, 1))
EastPrecipRCPDiff <- EastPrecipRCPDiff[, c(ncol(EastPrecipRCPDiff), 1:ncol(EastPrecipRCPDiff)-1)]
EastPrecipRCPDiff <- EastPrecipRCPDiff %>%
  filter(Year > 2009 & Year <= 2021)

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

EmploymentIDs <- list(Construction = 'CEU2000000002',
                      FramingContractors = 'CEU2023813002',
                      MasonryContractors = 'CEU2023814002',
                      RoofingContractors = 'CEU2023816002',
                      FlooringContractors = 'CEU2023833002',
                      SidingContractors = 'CEU2023817002',
                      ResidentialRemodelers = 'CEU2023611802',
                      FinishCarpentryContractors = 'CEU2023835002',
                      SitePrepContractors = 'CEU2023891002',
                      BldgFinishingContractors = 'CEU2023830002',
                      BldgEqpmtContractors = 'CEU2023820002',
                      SpecialtyTradeContractors = 'CEU2023800002',
                      UtilitySystemContractors = 'CEU2023710002',
                      NonResBldgConstruction = 'CEU2023620002',
                      ResBldgConstruction = 'CEU2023610002',
                      IndBldgConstruction = 'CEU2023621002',
                      BldgConstruction = 'CEU2023600002',
                      OtherBldgFinishingContractors = 'CEU2023839002',
                      OtherSpecialtyTradeContractors = 'CEU2023890002',
                      DrywallInsulationContractors = 'CEU2023831002',
                      GlassGlazingContractors = 'CEU2023815002',
                      OtherBldgEqpmtContractors = 'CEU2023829002',
                      TileTerrazoContractors = 'CEU2023834002',
                      AllOtherSpecialtyTradeContractors = 'CEU2023899002',
                      PaintingWallCoveringContractors = 'CEU2023832002',
                      CommercialInstitutionalBldgConstruction = 'CEU2023622002',
                      PouredConcreteFoundationStructureContractors = 'CEU2023811002',
                      PLumbingHVACContractors = 'CEU2023822002',
                      FoundationStructureBldgExteriorContractors = 'CEU2023810002',
                      StructuralSteelPrecastConcreteContractors = 'CEU2023812002',
                      OtherFoundationStructureBldgExteriorContractors = 'CEU2023819002',
                      ElectricalOtherWiringInstallationContractors = 'CEU2023821002',
                      WaterSewerLineRelatedStructureConstruction = 'CEU2023711002',
                      NewSFHousingConstruction_NoForSaleBuilders = 'CEU2023611502')

EmploymentData <- get_series_tables(series_ids = EmploymentIDs,
                                    api_key = '55c1f6ba5ffb44bfbd4d2eb29af72dab',
                                    start_year = 2006,
                                    end_year = 2021)

final_df <- data.frame()
df_list <- list()

for (name in names(EmploymentData)) {
  df <- as.data.frame(EmploymentData[[name]]) %>%
    mutate(period = gsub('M', '', period),
           date = as.Date(paste0(year, '-', period, '-01'))) %>%
    select(date, value) %>%
    as.xts() %>%
    apply.yearly(FUN = mean) %>%
    as.data.frame() %>%
    mutate(Year = c(seq(2006, 2021, 1)))

  # Store the dataframe in the list
  df_list[[name]] <- df
}

# Bind all dataframes in the list row-wise (by 'Year')
final_df <- bind_rows(df_list, .id = "name")

EmploymentData <- final_df %>%
  pivot_wider(names_from = name, values_from = value)

EmploymentData <- lapply(EmploymentData[,-1], percentage_diff)
EmploymentData <- as.data.frame(EmploymentData)
EmploymentData <- EmploymentData[-1,]
EmploymentData$Year <- c(seq(2007,2021,1))
EmploymentData <- EmploymentData[,c(35, 1:34)]






Correlations <- Correlations %>%
  filter(Variable != '') %>%
  mutate(Variable = ifelse(Variable == '', '', Variable))



write.xlsx(Correlations, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_Quarterly_2010_Current.xlsx")

sd_df <- data.frame(MiniPredictor1 = 1/sd(OutsideData$MasonryContractors),
                    MiniPredictor2 = 1/sd(OutsideData$FurnitureStores),
                    MiniPredictor3 = 1/sd(PermitData$DeckingResPermitCount),
                    MiniPredictor4 = 1/sd(OutsideData$ElectronicsandApplianceStores),

                    SmallPredictor1 = 1/sd(OutsideData$MasonryContractors),
                    SmallPredictor2 = 1/sd(PermitData$DeckingResPermitCount),
                    SmallPredictor3 = 1/sd(PermitData$PermitTypeMechanicalResPermitCount),
                    SmallPredictor4 = 1/sd(OutsideData$FurnitureStores),
                    SmallPredictor5 = 1/sd(HIRL_Data_Total$Insulation_Total),
                    SmallPredictor6 = 1/sd(HIRL_Data_RR$HVAC_RR),

                    MediumPredictor1 = 1/sd(OutsideData$FurnitureStores),
                    MediumPredictor2 = 1/sd(OutsideData$MasonryContractors),
                    MediumPredictor3 = 1/sd(OutsideData$ElectronicsandApplianceStores),
                    MediumPredictor4 = 1/sd(PermitData$ExistingStructureResPermitCount),
                    MediumPredictor5 = 1/sd(PermitData$PermitTypePlumbingResPermitCount),
                    MediumPredictor6 = 1/sd(HIRL_Data_RR$Drywall_Wallboard_RR),

                    LargePredictor1 = 1/sd(OutsideData$TotalConsumerCreditOwnedandSecuritized),
                    LargePredictor2 = 1/sd(OutsideData$`Burns.Home.Value.Index.YOY.%`),
                    LargePredictor3 = 1/sd(HIRL_Data_RR$Dimensional_Lumber_RR),
                    LargePredictor4 = 1/sd(OutsideData$BuildingMaterialsandSuppliesDealers),
                    LargePredictor5 = 1/sd(HIRL_Data_RR$Concrete_RR),
                    LargePredictor6 = 1/sd(HIRL_Data_RR$Sheathing_RR),
                    LargePredictor7 = 1/sd(HIRL_Data_RR$Doors_RR),
                    LargePredictor8 = 1/sd(HIRL_Data_RR$Flooring_RR),

                    PrecipPredictor1 = 1/sd(OutsideData$UtilitySystemContractors),
                    PrecipPredictor2 = 1/sd(OutsideData$GlassGlazingContractors))
sd_df <- sd_df %>%
  pivot_longer(cols = 1:ncol(sd_df)) %>%
  rename(`1/sd` = value)

cor_df <- data.frame(MiniPredictor1 = Correlations[Correlations$Variable == 'MasonryContractors',]$Mini,
                     MiniPredictor2 = Correlations[Correlations$Variable == 'FurnitureStores',]$Mini,
                     MiniPredictor3 = Correlations[Correlations$Variable == 'DeckingResPermitCount',]$Mini,
                     MiniPredictor4 = Correlations[Correlations$Variable == 'ElectronicsandApplianceStores',]$Mini,

                     SmallPredictor1 = Correlations[Correlations$Variable == 'MasonryContractors',]$Small,
                     SmallPredictor2 = Correlations[Correlations$Variable == 'DeckingResPermitCount',]$Small,
                     SmallPredictor3 = Correlations[Correlations$Variable == 'PermitTypeMechanicalResPermitCount',]$Small,
                     SmallPredictor4 = Correlations[Correlations$Variable == 'FurnitureStores',]$Small,
                     SmallPredictor5 = Correlations[Correlations$Variable == 'Insulation_Total',]$Small,
                     SmallPredictor6 = Correlations[Correlations$Variable == 'HVAC_RR',]$Small,

                     MediumPredictor1 = Correlations[Correlations$Variable == 'FurnitureStores',]$Medium,
                     MediumPredictor2 = Correlations[Correlations$Variable == 'MasonryContractors',]$Medium,
                     MediumPredictor3 = Correlations[Correlations$Variable == 'ElectronicsandApplianceStores',]$Medium,
                     MediumPredictor4 = Correlations[Correlations$Variable == 'ExistingStructureResPermitCount',]$Medium,
                     MediumPredictor5 = Correlations[Correlations$Variable == 'PermitTypePlumbingResPermitCount',]$Medium,
                     MediumPredictor6 = Correlations[Correlations$Variable == 'Drywall_Wallboard_RR',]$Medium,

                     LargePredictor1 = Correlations[Correlations$Variable == 'TotalConsumerCreditOwnedandSecuritized',]$Large,
                     LargePredictor2 = Correlations[Correlations$Variable == 'Burns.Home.Value.Index.YOY.%',]$Large,
                     LargePredictor3 = Correlations[Correlations$Variable == 'Dimensional_Lumber_RR',]$Large,
                     LargePredictor4 = Correlations[Correlations$Variable == 'BuildingMaterialsandSuppliesDealers',]$Large,
                     LargePredictor5 = Correlations[Correlations$Variable == 'Concrete_RR',]$Large,
                     LargePredictor6 = Correlations[Correlations$Variable == 'Sheathing_RR',]$Large,
                     LargePredictor7 = Correlations[Correlations$Variable == 'Doors_RR',]$Large,
                     LargePredictor8 = Correlations[Correlations$Variable == 'Flooring_RR',]$Large,

                     PrecipPredictor1 = Correlations[Correlations$Variable == 'UtilitySystemContractors',]$DisRepair,
                     PrecipPredictor2 = Correlations[Correlations$Variable == 'GlassGlazingContractors',]$DisRepair)


cor_df <- cor_df %>%
  pivot_longer(cols = 1:ncol(cor_df)) %>%
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

PredProps$Variable <- c('MasonryContractors',
                        'FurnitureStores',
                        'DeckingResPermitCount',
                        'ElectronicsandApplianceStores',

                        'MasonryContractors',
                        'DeckingResPermitCount',
                        'PermitTypeMechanicalResPermitCount',
                        'FurnitureStores',
                        'Insulation_Total',
                        'HVAC_RR',

                        'FurnitureStores',
                        'MasonryContractors',
                        'ElectronicsandApplianceStores',
                        'ExistingStructureResPermitCount',
                        'PermitTypePlumbingResPermitCount',
                        'Drywall_Wallboard_RR',

                        'TotalConsumerCreditOwnedandSecuritized',
                        'Burns.Home.Value.Index.YOY.%',
                        'Dimensional_Lumber_RR',
                        'BuildingMaterialsandSuppliesDealers',
                        'Concrete_RR',
                        'Sheathing_RR',
                        'Doors_RR',
                        'Flooring_RR',

                        'UtilitySystemContractors',
                        'GlassGlazingContractors')

dataset_list <- list(Correlations, PredProps)


write.xlsx(dataset_list, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_Quarterly_2010_Current.xlsx")
