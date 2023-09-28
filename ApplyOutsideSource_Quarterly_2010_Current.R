library(tidyverse)
library(fredr)
library(openxlsx)
library(lubridate)
library(xts)
library(blsR)
# Set the FRED API Key, if a new user is using this you will have to obtain an API key from here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Read in MRTS (ARTS) variables by searching for ids containing 'MRTS'
ARTSVariables <- fredr_series_search_id('MRTS') %>%
  # Filter for Not Seasno
  filter(seasonal_adjustment == 'Not Seasonally Adjusted' & units_short == 'Mil. of $' & frequency == 'Monthly' & as.Date(observation_end) >= '2021-12-01' & as.Date(observation_start) <= '1994-01-01') %>%
  select(id, title) %>%
  mutate(title =  gsub(" ", "", title)) %>%
  mutate(title =  gsub(":", "", title)) %>%
  mutate(title =  gsub(",", "_", title)) %>%
  mutate(title =  gsub("-", "_", title)) %>%
  mutate(title =  gsub("'", "", title)) %>%
  mutate(title =  gsub("RetailSales", "", title))

# Test <- fredr(series_id = 'MARTSMPCSM44000USS',  observation_start = as.Date('1994-01-01'), observation_end = as.Date('2021-01-01'), frequency = 'm')

# Create an empty list to store the data frames
dataframes_list <- list()

for (i in 1:length(ARTSVariables$id)) {
  # Fetch the data using fredr
  data <- fredr(series_id = c(ARTSVariables$id[i]), sort_order = 'asc', frequency = 'q', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-12-01')) %>%
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
  rename_with(~gsub("\\.value", "", .), ends_with('.value'))

ARTSData <- ARTSData %>%
  rename(Date = RetailTrade.date)

ARTSDates <- ARTSData$Date

ARTSData <- ARTSData %>%
  mutate(Date = quarter(Date, with_year = TRUE))

Inflation <- fredr(series_id = 'CPIAUCSL', frequency = 'q', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-12-01')) %>%
  select(date, value)

Inflation <- Inflation %>%
  mutate(date = quarter(date, with_year = TRUE))

CPI2021 <- Inflation %>%
  filter(date == '2021.4') %>%
  select(value)
CPI2021 <- CPI2021$value
Inflation <- Inflation %>%
  mutate(value = CPI2021/value) %>%
  rename(Date = date, Rate = value)

ARTSData <- ARTSData %>%
  left_join(Inflation, by = 'Date')

ARTSCols <- as.numeric(ncol(ARTSData)-1)
ARTSData[,2:ARTSCols] <- ARTSData[,2:ARTSCols]*ARTSData$Rate

ARTSData <- ARTSData %>%
  select(-Rate)

ARTSData <- ARTSData %>%
  mutate(DateChar = as.character(Date))

ARTSData$Year <- str_extract(ARTSData$DateChar, pattern = '\\d{4}')
ARTSData$Year <- as.numeric(ARTSData$Year)

ARTSData <- ARTSData %>%
  select(-DateChar)

ARTSData <- ARTSData[, c(1,ARTSCols+1, 2:ARTSCols)]


OtherVariables <- fredr_series_search_text(search_text = 'Monthly', filter_variable = 'seasonal_adjustment')
OtherVariables <- OtherVariables %>%
  filter(frequency == 'Monthly' & units_short %in% c('Mil. of $', 'Bil. of U.S. $', 'Bil. of $')) %>%
  mutate(observation_end = as.Date(observation_end),
         observation_start = as.Date(observation_start)) %>%
  filter(observation_end >= '2021-12-01' & observation_start <= '1993-01-01') %>%
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
  data <- fredr(series_id = c(OtherVariables$id[i]), sort_order = 'asc', frequency = 'q', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-12-01')) %>%
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
  rename_with(~gsub("\\.value", "", .), ends_with('.value'))

OtherData <- OtherData %>%
  rename(Date = M2.date)

OtherData$Date <- quarter(OtherData$Date, with_year = TRUE)
OtherData <- OtherData %>%
  left_join(Inflation, by = 'Date')

OtherDataCols <- as.numeric(ncol(OtherData)-1)
OtherData[,2:OtherDataCols] <- OtherData[,2:OtherDataCols]*OtherData$Rate

OtherData <- OtherData %>%
  select(-Rate)

OtherData <- OtherData %>%
  mutate(DateChar = as.character(Date))

OtherData$Year <- str_extract(OtherData$DateChar, pattern = '\\d{4}')
OtherData$Year <- as.numeric(OtherData$Year)

OtherData <- OtherData %>%
  select(-DateChar)

OtherData <- OtherData[, c(1, OtherDataCols+1, 2:OtherDataCols)]


OutsideData <- ARTSData %>%
  left_join(OtherData[,-2], by = 'Date')


# LumberVariables <- fredr_series_search_text('lumber') %>%
#   filter(seasonal_adjustment == 'Not Seasonally Adjusted' & frequency == 'Monthly' & as.Date(observation_start) <= '1993-01-01' & str_detect(units, 'Index') & as.Date(observation_end) >= '2021-12-01') %>%
#   select(id, title) %>%
#   mutate(title =  gsub(" ", "", title)) %>%
#   mutate(title =  gsub(":", "", title)) %>%
#   mutate(title =  gsub(",", "_", title)) %>%
#   mutate(title =  gsub("-", "_", title)) %>%
#   mutate(title =  gsub("'", "", title))
#
# # Create an empty list to store the data frames
# dataframes_list <- list()
#
# for (i in 1:length(LumberVariables$id)) {
#   # Fetch the data using fredr
#   data <- fredr(series_id = c(LumberVariables$id[i]), sort_order = 'asc', frequency = 'q', observation_start = as.Date('1993-01-01'), observation_end = as.Date('2021-12-01')) %>%
#     select(date, value)
#
#   # Set the column name to the AmendedLabel
#   col_name <- LumberVariables$title[i]
#
#   # Store the data frame in the list with the appropriate column name
#   dataframes_list[[col_name]] <- data
# }
#
# # Combine all data frames into a single data frame
# LumberData <- do.call(cbind, dataframes_list)
#
# # Select only the value columns along with a date column
# LumberData <- LumberData %>%
#   select(ProducerPriceIndexbyCommodityPulp_Paper_andAlliedProductsWoodPulp.date, ends_with('.value'))
#
# # Rename columns by dropping '.value'
# LumberData <- LumberData %>%
#   rename_with(~gsub(".value", "", .), ends_with('.value'))
#
# LumberData <- LumberData %>%
#   rename(Date = ProducerPriceIndexbyCommodityPulp_Paper_andAlliedProductsWoodPulp.date)
#
# LumberDates <- LumberData$Date
#
# LumberData$Date <- quarter(LumberData$Date, with_year = TRUE)
#
# LumberData <- LumberData %>%
#   left_join(Inflation, by = 'Date')
#
# LumberDataCols <- as.numeric(ncol(LumberData)-1)
# LumberData[,2:LumberDataCols] <- LumberData[,2:LumberDataCols]*LumberData$Rate
#
# LumberData <- LumberData %>%
#   select(-Rate)
#
# LumberData <- LumberData %>%
#   mutate(DateChar = as.character(Date))
#
# LumberData$Year <- str_extract(LumberData$DateChar, pattern = '\\d{4}')
# LumberData$Year <- as.numeric(LumberData$Year)
#
# LumberData <- LumberData %>%
#   select(-DateChar)
#
# LumberData <- LumberData[, c(1, LumberDataCols+1, 2:LumberDataCols)]
#
#
# OutsideData <- OutsideData %>%
#   left_join(LumberData[,-2], by = 'Date')

###

PermitData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/JohnBurns_PermitCounts_20230717.xlsx")
PermitDates <- unique(PermitData$Year)
PermitData$Quarter <- paste0(PermitData$Year, PermitData$Quarter)
PermitData$Quarter <-  as.numeric(str_replace(PermitData$Quarter, pattern = 'Q', replacement = '.'))
PermitData <- PermitData %>%
  rename(Date = Quarter)
OutsideData <- OutsideData %>%
  left_join(PermitData[,-1], by = 'Date')


HIRLData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/HIRLData.xlsx", sheet = 1)
HIRLDates <- unique(HIRLData$Year)
HIRLData$Quarter <- as.numeric(paste0(HIRLData$Year, '.', HIRLData$Quarter))
HIRLData <- HIRLData %>%
  rename(Date = Quarter)
HIRL_Years_Dates <- HIRLData[,1:2]
HIRLData <- HIRLData[,3:20]
names(HIRLData) <- paste0(names(HIRLData), '_RR')
HIRLData <- HIRLData %>%
  cbind(HIRL_Years_Dates)
HIRLData <- HIRLData[, c(19:20, 1:18)]
OutsideData <- OutsideData %>%
  left_join(HIRLData[,-1], by = 'Date')

HIRLData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/HIRLData.xlsx", sheet = 2)
HIRLDates <- unique(HIRLData$Year)
HIRLData$Quarter <- as.numeric(paste0(HIRLData$Year, '.', HIRLData$Quarter))
HIRLData <- HIRLData %>%
  rename(Date = Quarter)
HIRL_Years_Dates <- HIRLData[,1:2]
HIRLData <- HIRLData[,3:20]
names(HIRLData) <- paste0(names(HIRLData), '_Total')
HIRLData <- HIRLData %>%
  cbind(HIRL_Years_Dates)
HIRLData <- HIRLData[, c(19:20, 1:18)]
OutsideData <- OutsideData %>%
  left_join(HIRLData[,-1], by = 'Date')
###

OutsideData <- OutsideData %>%
  filter(Date >= '1994-01-01')

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



# Create a 'MiniPredictor' column in the template dataframe containing the variable to use for Mini (MajorAppliances_40), then join the 2-Year Mini data to the template by year
## Do the same for each of the other cost classes. For Disaster Repairs paste both of the predictor columns
OutsideData <- OutsideData %>%
  left_join(AllData, by = 'Year')

PrecipData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USEast_TotalPrecipitation_Monthly.xlsx")
PrecipData$Date <- as.Date(paste0(PrecipData$Date, '01'), format = '%Y%m%d')
PrecipData <- as.xts(PrecipData)

PrecipData <- apply.quarterly(PrecipData, FUN = sum)
PrecipData$Quarter <- quarter(PrecipData)
PrecipData$Year <- year(PrecipData)
PrecipData <- as.data.frame(PrecipData)
PrecipData$Date <- as.numeric(paste0(PrecipData$Year, '.', PrecipData$Quarter))
PrecipData <- PrecipData[, c(3,4,1)]
PrecipData <- PrecipData %>%
  rename(Ann_CR_EastUS = Precip)

OutsideData <- OutsideData %>%
  left_join(PrecipData, by = 'Date')



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
    apply.quarterly(FUN = mean) %>%
    as.data.frame()

  # Store the dataframe in the list
  df_list[[name]] <- df
}

# Bind all dataframes in the list row-wise (by 'Year')
final_df <- bind_rows(df_list, .id = "name")

final_df <- rownames_to_column(final_df)
final_df <- final_df %>%
  rename(Date = rowname)

final_df$Date <- str_extract(final_df$Date, pattern = '\\d{4}-\\d{2}-\\d{2}')
EmploymentData <- final_df %>%
  pivot_wider(names_from = name, values_from = value)

EmploymentData$Date <- as.Date(EmploymentData$Date)
EmploymentData$Year <- year(EmploymentData$Date)
EmploymentData$Quarter <- quarter(EmploymentData$Date, with_year = FALSE)
EmploymentData$Date <- as.numeric(paste0(EmploymentData$Year, '.', EmploymentData$Quarter))

EmploymentData <- EmploymentData %>%
  select(-c(Year, Quarter))


OutsideData <- OutsideData %>%
  left_join(EmploymentData, by = 'Date')

BHVIData <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/BHVI_Data-2023_09.xlsx")

BHVIData <- BHVIData %>%
  pivot_longer(cols = 1:264)
BHVIData$name <- as.Date(as.numeric(BHVIData$name), origin = "1900-01-01")
BHVIData <- BHVIData %>%
  rename(Date = name, BHVI = value)

BHVIData <- as.xts(BHVIData)
BHVIData <- apply.quarterly(BHVIData, FUN = mean)
BHVIData <- as.data.frame(BHVIData)
BHVIData <- rownames_to_column(BHVIData)
BHVIData <- BHVIData %>%
  rename(Date = rowname) %>%
  mutate(Date = as.Date(Date),
         Quarter = quarter(Date),
         Year = year(Date),
         Date = as.numeric(paste0(Year, '.', Quarter))) %>%
  select(Date, BHVI) %>%
  left_join(Inflation, by = 'Date') %>%
  mutate(BHVI = BHVI*Rate) %>%
  select(-Rate)

OutsideData <- OutsideData %>%
  left_join(BHVIData, by = 'Date')

OutsideData <- OutsideData %>%
  mutate(MiniPredictor1 = MasonryContractors,
         MiniPredictor2 = FurnitureStores,
         MiniPredictor3 = DeckingResPermitCount,
         MiniPredictor4 = ElectronicsandApplianceStores,

         SmallPredictor1 = MasonryContractors,
         SmallPredictor2 = DeckingResPermitCount,
         SmallPredictor3 = PermitTypeMechanicalResPermitCount,
         SmallPredictor4 = FurnitureStores,
         SmallPredictor5 = Insulation_Total,
         SmallPredictor6 = HVAC_RR,

         MediumPredictor1 = FurnitureStores,
         MediumPredictor2 = MasonryContractors,
         MediumPredictor3 = ElectronicsandApplianceStores,
         MediumPredictor4 = ExistingStructureResPermitCount,
         MediumPredictor5 = PermitTypePlumbingResPermitCount,
         MediumPredictor6 = Drywall_Wallboard_RR,

         LargePredictor1 = TotalConsumerCreditOwnedandSecuritized,
         LargePredictor2 = BHVI,
         LargePredictor3 = Dimensional_Lumber_RR,
         LargePredictor4 = BuildingMaterialsandSuppliesDealers,
         LargePredictor5 = Concrete_RR,
         LargePredictor6 = Sheathing_RR,
         LargePredictor7 = Doors_RR,
         LargePredictor8 = Flooring_RR,

         PrecipPredictor1 = UtilitySystemContractors,
         PrecipPredictor2 = GlassGlazingContractors)

OutsideData <- OutsideData %>%
  select(Date, Year.x, Mini, Small, Medium, Large, DisRepair, MiniPredictor1:PrecipPredictor2) %>%
  rename(Year = Year.x)

# Create an empty template dataframe, where Year is equal to 1994-2021 and all other columns are NA (thses will be filled soon!)
Template <- data.frame(Date = OutsideData$Date,
                       Year = OutsideData$Year,
                       Mini = NA,
                       Small = NA,
                       Medium = NA,
                       Large = NA,
                       DisRepairs = NA)




PredProps <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_Quarterly_2010_Current.xlsx", sheet = 2)
PredProps <- PredProps %>%
  select(name, Variable, FinalProp)

MiniPredictor1_Mult <- PredProps[PredProps$name == 'MiniPredictor1',]$FinalProp
MiniPredictor2_Mult <- PredProps[PredProps$name == 'MiniPredictor2',]$FinalProp
MiniPredictor3_Mult <- PredProps[PredProps$name == 'MiniPredictor3',]$FinalProp
MiniPredictor4_Mult <- PredProps[PredProps$name == 'MiniPredictor4',]$FinalProp

SmallPredictor1_Mult <- PredProps[PredProps$name == 'SmallPredictor1',]$FinalProp
SmallPredictor2_Mult <- PredProps[PredProps$name == 'SmallPredictor2',]$FinalProp
SmallPredictor3_Mult <- PredProps[PredProps$name == 'SmallPredictor3',]$FinalProp
SmallPredictor4_Mult <- PredProps[PredProps$name == 'SmallPredictor4',]$FinalProp
SmallPredictor5_Mult <- PredProps[PredProps$name == 'SmallPredictor5',]$FinalProp
SmallPredictor6_Mult <- PredProps[PredProps$name == 'SmallPredictor6',]$FinalProp

MediumPredictor1_Mult <- PredProps[PredProps$name == 'MediumPredictor1',]$FinalProp
MediumPredictor2_Mult <- PredProps[PredProps$name == 'MediumPredictor2',]$FinalProp
MediumPredictor3_Mult <- PredProps[PredProps$name == 'MediumPredictor3',]$FinalProp
MediumPredictor4_Mult <- PredProps[PredProps$name == 'MediumPredictor4',]$FinalProp
MediumPredictor5_Mult <- PredProps[PredProps$name == 'MediumPredictor5',]$FinalProp
MediumPredictor6_Mult <- PredProps[PredProps$name == 'MediumPredictor6',]$FinalProp

LargePredictor1_Mult <- PredProps[PredProps$name == 'LargePredictor1',]$FinalProp
LargePredictor2_Mult <- PredProps[PredProps$name == 'LargePredictor2',]$FinalProp
LargePredictor3_Mult <- PredProps[PredProps$name == 'LargePredictor3',]$FinalProp
LargePredictor4_Mult <- PredProps[PredProps$name == 'LargePredictor4',]$FinalProp
LargePredictor5_Mult <- PredProps[PredProps$name == 'LargePredictor5',]$FinalProp
LargePredictor6_Mult <- PredProps[PredProps$name == 'LargePredictor6',]$FinalProp
LargePredictor7_Mult <- PredProps[PredProps$name == 'LargePredictor7',]$FinalProp
LargePredictor8_Mult <- PredProps[PredProps$name == 'LargePredictor8',]$FinalProp

PrecipPredictor1_Mult <- PredProps[PredProps$name == 'PrecipPredictor1',]$FinalProp
PrecipPredictor2_Mult <- PredProps[PredProps$name == 'PrecipPredictor2',]$FinalProp


OutsideData <- OutsideData %>%
  filter(Year >= 2010)

Template <- Template %>%
  filter(Date >= 2010)

# For each year from 1994-current
for (i in 1:length(OutsideData$Date)){

  # If on calculating the first quarter in a given year, take the current predictor value and divide by the sum of the current value and the next three values (i.e. Q1/sum(Q1-Q4))
  ## This gives the given quarter's proportion factor, which is then multiplied by the yearly spend for the category in question
  if (str_detect(OutsideData$Date[i], pattern = '1$')){

    Template$Mini[i] <- ((OutsideData$Mini[i] * (OutsideData$MiniPredictor1[i] / (OutsideData$MiniPredictor1[i] + OutsideData$MiniPredictor1[i+1] + OutsideData$MiniPredictor1[i+2] + OutsideData$MiniPredictor1[i+3]))) * MiniPredictor1_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor2[i] / (OutsideData$MiniPredictor2[i] + OutsideData$MiniPredictor2[i+1] + OutsideData$MiniPredictor2[i+2] + OutsideData$MiniPredictor2[i+3]))) * MiniPredictor2_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor3[i] / (OutsideData$MiniPredictor3[i] + OutsideData$MiniPredictor3[i+1] + OutsideData$MiniPredictor3[i+2] + OutsideData$MiniPredictor3[i+3]))) * MiniPredictor3_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor4[i] / (OutsideData$MiniPredictor4[i] + OutsideData$MiniPredictor4[i+1] + OutsideData$MiniPredictor4[i+2] + OutsideData$MiniPredictor4[i+3]))) * MiniPredictor4_Mult)


    Template$Small[i] <-  ((OutsideData$Small[i] * (OutsideData$SmallPredictor1[i] / (OutsideData$SmallPredictor1[i] + OutsideData$SmallPredictor1[i+1] + OutsideData$SmallPredictor1[i+2] + OutsideData$SmallPredictor1[i+3]))) * SmallPredictor1_Mult) +
                          ((OutsideData$Small[i] * (OutsideData$SmallPredictor2[i] / (OutsideData$SmallPredictor2[i] + OutsideData$SmallPredictor2[i+1] + OutsideData$SmallPredictor2[i+2] + OutsideData$SmallPredictor2[i+3]))) * SmallPredictor2_Mult) +
                          ((OutsideData$Small[i] * (OutsideData$SmallPredictor3[i] / (OutsideData$SmallPredictor3[i] + OutsideData$SmallPredictor3[i+1] + OutsideData$SmallPredictor3[i+2] + OutsideData$SmallPredictor3[i+3]))) * SmallPredictor3_Mult) +
                          ((OutsideData$Small[i] * (OutsideData$SmallPredictor4[i] / (OutsideData$SmallPredictor4[i] + OutsideData$SmallPredictor4[i+1] + OutsideData$SmallPredictor4[i+2] + OutsideData$SmallPredictor4[i+3]))) * SmallPredictor4_Mult) +
                          ((OutsideData$Small[i] * (OutsideData$SmallPredictor5[i] / (OutsideData$SmallPredictor5[i] + OutsideData$SmallPredictor5[i+1] + OutsideData$SmallPredictor5[i+2] + OutsideData$SmallPredictor5[i+3]))) * SmallPredictor5_Mult) +
                          ((OutsideData$Small[i] * (OutsideData$SmallPredictor6[i] / (OutsideData$SmallPredictor6[i] + OutsideData$SmallPredictor6[i+1] + OutsideData$SmallPredictor6[i+2] + OutsideData$SmallPredictor6[i+3]))) * SmallPredictor6_Mult)


    Template$Medium[i] <- ((OutsideData$Medium[i] * (OutsideData$MediumPredictor1[i] / (OutsideData$MediumPredictor1[i] + OutsideData$MediumPredictor1[i+1] + OutsideData$MediumPredictor1[i+2] + OutsideData$MediumPredictor1[i+3]))) * MediumPredictor1_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor2[i] / (OutsideData$MediumPredictor2[i] + OutsideData$MediumPredictor2[i+1] + OutsideData$MediumPredictor2[i+2] + OutsideData$MediumPredictor2[i+3]))) * MediumPredictor2_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor3[i] / (OutsideData$MediumPredictor3[i] + OutsideData$MediumPredictor3[i+1] + OutsideData$MediumPredictor3[i+2] + OutsideData$MediumPredictor3[i+3]))) * MediumPredictor3_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor4[i] / (OutsideData$MediumPredictor4[i] + OutsideData$MediumPredictor4[i+1] + OutsideData$MediumPredictor4[i+2] + OutsideData$MediumPredictor4[i+3]))) * MediumPredictor4_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor5[i] / (OutsideData$MediumPredictor5[i] + OutsideData$MediumPredictor5[i+1] + OutsideData$MediumPredictor5[i+2] + OutsideData$MediumPredictor5[i+3]))) * MediumPredictor5_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor6[i] / (OutsideData$MediumPredictor6[i] + OutsideData$MediumPredictor6[i+1] + OutsideData$MediumPredictor6[i+2] + OutsideData$MediumPredictor6[i+3]))) * MediumPredictor6_Mult)

    Template$Large[i] <-  ((OutsideData$Large[i] * (OutsideData$LargePredictor1[i] / (OutsideData$LargePredictor1[i] + OutsideData$LargePredictor1[i+1] + OutsideData$LargePredictor1[i+2] + OutsideData$LargePredictor1[i+3]))) * LargePredictor1_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor2[i] / (OutsideData$LargePredictor2[i] + OutsideData$LargePredictor2[i+1] + OutsideData$LargePredictor2[i+2] + OutsideData$LargePredictor2[i+3]))) * LargePredictor2_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor3[i] / (OutsideData$LargePredictor3[i] + OutsideData$LargePredictor3[i+1] + OutsideData$LargePredictor3[i+2] + OutsideData$LargePredictor3[i+3]))) * LargePredictor3_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor4[i] / (OutsideData$LargePredictor4[i] + OutsideData$LargePredictor4[i+1] + OutsideData$LargePredictor4[i+2] + OutsideData$LargePredictor4[i+3]))) * LargePredictor4_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor5[i] / (OutsideData$LargePredictor5[i] + OutsideData$LargePredictor5[i+1] + OutsideData$LargePredictor5[i+2] + OutsideData$LargePredictor5[i+3]))) * LargePredictor5_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor6[i] / (OutsideData$LargePredictor6[i] + OutsideData$LargePredictor6[i+1] + OutsideData$LargePredictor6[i+2] + OutsideData$LargePredictor6[i+3]))) * LargePredictor6_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor7[i] / (OutsideData$LargePredictor7[i] + OutsideData$LargePredictor7[i+1] + OutsideData$LargePredictor7[i+2] + OutsideData$LargePredictor7[i+3]))) * LargePredictor7_Mult) +
                          ((OutsideData$Large[i] * (OutsideData$LargePredictor8[i] / (OutsideData$LargePredictor8[i] + OutsideData$LargePredictor8[i+1] + OutsideData$LargePredictor8[i+2] + OutsideData$LargePredictor8[i+3]))) * LargePredictor8_Mult)

    Template$DisRepairs[i] <- ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor1[i] / (OutsideData$PrecipPredictor1[i] + OutsideData$PrecipPredictor1[i+1] + OutsideData$PrecipPredictor1[i+2] + OutsideData$PrecipPredictor1[i+3]))) * PrecipPredictor1_Mult) +
                              ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor2[i] / (OutsideData$PrecipPredictor2[i] + OutsideData$PrecipPredictor2[i+1] + OutsideData$PrecipPredictor2[i+2] + OutsideData$PrecipPredictor2[i+3]))) * PrecipPredictor2_Mult)

  }

  else if (str_detect(OutsideData$Date[i], pattern = '2$')){

    Template$Mini[i] <- ((OutsideData$Mini[i] * (OutsideData$MiniPredictor1[i] / (OutsideData$MiniPredictor1[i] + OutsideData$MiniPredictor1[i-1] + OutsideData$MiniPredictor1[i+1] + OutsideData$MiniPredictor1[i+2]))) * MiniPredictor1_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor2[i] / (OutsideData$MiniPredictor2[i] + OutsideData$MiniPredictor2[i-1] + OutsideData$MiniPredictor2[i+1] + OutsideData$MiniPredictor2[i+2]))) * MiniPredictor2_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor3[i] / (OutsideData$MiniPredictor3[i] + OutsideData$MiniPredictor3[i-1] + OutsideData$MiniPredictor3[i+1] + OutsideData$MiniPredictor3[i+2]))) * MiniPredictor3_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor4[i] / (OutsideData$MiniPredictor4[i] + OutsideData$MiniPredictor4[i-1] + OutsideData$MiniPredictor4[i+1] + OutsideData$MiniPredictor4[i+2]))) * MiniPredictor4_Mult)

    Template$Small[i] <- ((OutsideData$Small[i] * (OutsideData$SmallPredictor1[i] / (OutsideData$SmallPredictor1[i] + OutsideData$SmallPredictor1[i-1] + OutsideData$SmallPredictor1[i+1] + OutsideData$SmallPredictor1[i+2]))) * SmallPredictor1_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor2[i] / (OutsideData$SmallPredictor2[i] + OutsideData$SmallPredictor2[i-1] + OutsideData$SmallPredictor2[i+1] + OutsideData$SmallPredictor2[i+2]))) * SmallPredictor2_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor3[i] / (OutsideData$SmallPredictor3[i] + OutsideData$SmallPredictor3[i-1] + OutsideData$SmallPredictor3[i+1] + OutsideData$SmallPredictor3[i+2]))) * SmallPredictor3_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor4[i] / (OutsideData$SmallPredictor4[i] + OutsideData$SmallPredictor4[i-1] + OutsideData$SmallPredictor4[i+1] + OutsideData$SmallPredictor4[i+2]))) * SmallPredictor4_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor5[i] / (OutsideData$SmallPredictor5[i] + OutsideData$SmallPredictor5[i-1] + OutsideData$SmallPredictor5[i+1] + OutsideData$SmallPredictor5[i+2]))) * SmallPredictor5_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor6[i] / (OutsideData$SmallPredictor6[i] + OutsideData$SmallPredictor6[i-1] + OutsideData$SmallPredictor6[i+1] + OutsideData$SmallPredictor6[i+2]))) * SmallPredictor6_Mult)

    Template$Medium[i] <- ((OutsideData$Medium[i] * (OutsideData$MediumPredictor1[i] / (OutsideData$MediumPredictor1[i] + OutsideData$MediumPredictor1[i-1] + OutsideData$MediumPredictor1[i+1] + OutsideData$MediumPredictor1[i+2]))) * MediumPredictor1_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor2[i] / (OutsideData$MediumPredictor2[i] + OutsideData$MediumPredictor2[i-1] + OutsideData$MediumPredictor2[i+1] + OutsideData$MediumPredictor2[i+2]))) * MediumPredictor2_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor3[i] / (OutsideData$MediumPredictor3[i] + OutsideData$MediumPredictor3[i-1] + OutsideData$MediumPredictor3[i+1] + OutsideData$MediumPredictor3[i+2]))) * MediumPredictor3_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor4[i] / (OutsideData$MediumPredictor4[i] + OutsideData$MediumPredictor4[i-1] + OutsideData$MediumPredictor4[i+1] + OutsideData$MediumPredictor4[i+2]))) * MediumPredictor4_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor5[i] / (OutsideData$MediumPredictor5[i] + OutsideData$MediumPredictor5[i-1] + OutsideData$MediumPredictor5[i+1] + OutsideData$MediumPredictor5[i+2]))) * MediumPredictor5_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor6[i] / (OutsideData$MediumPredictor6[i] + OutsideData$MediumPredictor6[i-1] + OutsideData$MediumPredictor6[i+1] + OutsideData$MediumPredictor6[i+2]))) * MediumPredictor6_Mult)

    Template$Large[i] <- ((OutsideData$Large[i] * (OutsideData$LargePredictor1[i] / (OutsideData$LargePredictor1[i] + OutsideData$LargePredictor1[i-1] + OutsideData$LargePredictor1[i+1] + OutsideData$LargePredictor1[i+2]))) * LargePredictor1_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor2[i] / (OutsideData$LargePredictor2[i] + OutsideData$LargePredictor2[i-1] + OutsideData$LargePredictor2[i+1] + OutsideData$LargePredictor2[i+2]))) * LargePredictor2_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor3[i] / (OutsideData$LargePredictor3[i] + OutsideData$LargePredictor3[i-1] + OutsideData$LargePredictor3[i+1] + OutsideData$LargePredictor3[i+2]))) * LargePredictor3_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor4[i] / (OutsideData$LargePredictor4[i] + OutsideData$LargePredictor4[i-1] + OutsideData$LargePredictor4[i+1] + OutsideData$LargePredictor4[i+2]))) * LargePredictor4_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor5[i] / (OutsideData$LargePredictor5[i] + OutsideData$LargePredictor5[i-1] + OutsideData$LargePredictor5[i+1] + OutsideData$LargePredictor5[i+2]))) * LargePredictor5_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor6[i] / (OutsideData$LargePredictor6[i] + OutsideData$LargePredictor6[i-1] + OutsideData$LargePredictor6[i+1] + OutsideData$LargePredictor6[i+2]))) * LargePredictor6_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor7[i] / (OutsideData$LargePredictor7[i] + OutsideData$LargePredictor7[i-1] + OutsideData$LargePredictor7[i+1] + OutsideData$LargePredictor7[i+2]))) * LargePredictor7_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor8[i] / (OutsideData$LargePredictor8[i] + OutsideData$LargePredictor8[i-1] + OutsideData$LargePredictor8[i+1] + OutsideData$LargePredictor8[i+2]))) * LargePredictor8_Mult)

    Template$DisRepairs[i] <- ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor1[i] / (OutsideData$PrecipPredictor1[i] + OutsideData$PrecipPredictor1[i-1] + OutsideData$PrecipPredictor1[i+1] + OutsideData$PrecipPredictor1[i+2]))) * PrecipPredictor1_Mult) +
                              ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor2[i] / (OutsideData$PrecipPredictor2[i] + OutsideData$PrecipPredictor2[i-1] + OutsideData$PrecipPredictor2[i+1] + OutsideData$PrecipPredictor2[i+2]))) * PrecipPredictor2_Mult)
  }

  else if (str_detect(OutsideData$Date[i], pattern = '3$')){

    Template$Mini[i] <- ((OutsideData$Mini[i] * (OutsideData$MiniPredictor1[i] / (OutsideData$MiniPredictor1[i] + OutsideData$MiniPredictor1[i-2] + OutsideData$MiniPredictor1[i-1] + OutsideData$MiniPredictor1[i+1]))) * MiniPredictor1_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor2[i] / (OutsideData$MiniPredictor2[i] + OutsideData$MiniPredictor2[i-2] + OutsideData$MiniPredictor2[i-1] + OutsideData$MiniPredictor2[i+1]))) * MiniPredictor2_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor3[i] / (OutsideData$MiniPredictor3[i] + OutsideData$MiniPredictor3[i-2] + OutsideData$MiniPredictor3[i-1] + OutsideData$MiniPredictor3[i+1]))) * MiniPredictor3_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor4[i] / (OutsideData$MiniPredictor4[i] + OutsideData$MiniPredictor4[i-2] + OutsideData$MiniPredictor4[i-1] + OutsideData$MiniPredictor4[i+1]))) * MiniPredictor4_Mult)

    Template$Small[i] <- ((OutsideData$Small[i] * (OutsideData$SmallPredictor1[i] / (OutsideData$SmallPredictor1[i] + OutsideData$SmallPredictor1[i-2] + OutsideData$SmallPredictor1[i-1] + OutsideData$SmallPredictor1[i+1]))) * SmallPredictor1_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor2[i] / (OutsideData$SmallPredictor2[i] + OutsideData$SmallPredictor2[i-2] + OutsideData$SmallPredictor2[i-1] + OutsideData$SmallPredictor2[i+1]))) * SmallPredictor2_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor3[i] / (OutsideData$SmallPredictor3[i] + OutsideData$SmallPredictor3[i-2] + OutsideData$SmallPredictor3[i-1] + OutsideData$SmallPredictor3[i+1]))) * SmallPredictor3_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor4[i] / (OutsideData$SmallPredictor4[i] + OutsideData$SmallPredictor4[i-2] + OutsideData$SmallPredictor4[i-1] + OutsideData$SmallPredictor4[i+1]))) * SmallPredictor4_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor5[i] / (OutsideData$SmallPredictor5[i] + OutsideData$SmallPredictor5[i-2] + OutsideData$SmallPredictor5[i-1] + OutsideData$SmallPredictor5[i+1]))) * SmallPredictor5_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor6[i] / (OutsideData$SmallPredictor6[i] + OutsideData$SmallPredictor6[i-2] + OutsideData$SmallPredictor6[i-1] + OutsideData$SmallPredictor6[i+1]))) * SmallPredictor6_Mult)

    Template$Medium[i] <- ((OutsideData$Medium[i] * (OutsideData$MediumPredictor1[i] / (OutsideData$MediumPredictor1[i] + OutsideData$MediumPredictor1[i-2] + OutsideData$MediumPredictor1[i-1] + OutsideData$MediumPredictor1[i+1]))) * MediumPredictor1_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor2[i] / (OutsideData$MediumPredictor2[i] + OutsideData$MediumPredictor2[i-2] + OutsideData$MediumPredictor2[i-1] + OutsideData$MediumPredictor2[i+1]))) * MediumPredictor2_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor3[i] / (OutsideData$MediumPredictor3[i] + OutsideData$MediumPredictor3[i-2] + OutsideData$MediumPredictor3[i-1] + OutsideData$MediumPredictor3[i+1]))) * MediumPredictor3_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor4[i] / (OutsideData$MediumPredictor4[i] + OutsideData$MediumPredictor4[i-2] + OutsideData$MediumPredictor4[i-1] + OutsideData$MediumPredictor4[i+1]))) * MediumPredictor4_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor5[i] / (OutsideData$MediumPredictor5[i] + OutsideData$MediumPredictor5[i-2] + OutsideData$MediumPredictor5[i-1] + OutsideData$MediumPredictor5[i+1]))) * MediumPredictor5_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor6[i] / (OutsideData$MediumPredictor6[i] + OutsideData$MediumPredictor6[i-2] + OutsideData$MediumPredictor6[i-1] + OutsideData$MediumPredictor6[i+1]))) * MediumPredictor6_Mult)

    Template$Large[i] <- ((OutsideData$Large[i] * (OutsideData$LargePredictor1[i] / (OutsideData$LargePredictor1[i] + OutsideData$LargePredictor1[i-2] + OutsideData$LargePredictor1[i-1] + OutsideData$LargePredictor1[i+1]))) * LargePredictor1_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor2[i] / (OutsideData$LargePredictor2[i] + OutsideData$LargePredictor2[i-2] + OutsideData$LargePredictor2[i-1] + OutsideData$LargePredictor2[i+1]))) * LargePredictor2_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor3[i] / (OutsideData$LargePredictor3[i] + OutsideData$LargePredictor3[i-2] + OutsideData$LargePredictor3[i-1] + OutsideData$LargePredictor3[i+1]))) * LargePredictor3_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor4[i] / (OutsideData$LargePredictor4[i] + OutsideData$LargePredictor4[i-2] + OutsideData$LargePredictor4[i-1] + OutsideData$LargePredictor4[i+1]))) * LargePredictor4_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor5[i] / (OutsideData$LargePredictor5[i] + OutsideData$LargePredictor5[i-2] + OutsideData$LargePredictor5[i-1] + OutsideData$LargePredictor5[i+1]))) * LargePredictor5_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor6[i] / (OutsideData$LargePredictor6[i] + OutsideData$LargePredictor6[i-2] + OutsideData$LargePredictor6[i-1] + OutsideData$LargePredictor6[i+1]))) * LargePredictor6_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor7[i] / (OutsideData$LargePredictor7[i] + OutsideData$LargePredictor7[i-2] + OutsideData$LargePredictor7[i-1] + OutsideData$LargePredictor7[i+1]))) * LargePredictor7_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor8[i] / (OutsideData$LargePredictor8[i] + OutsideData$LargePredictor8[i-2] + OutsideData$LargePredictor8[i-1] + OutsideData$LargePredictor8[i+1]))) * LargePredictor8_Mult)

    Template$DisRepairs[i] <- ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor1[i] / (OutsideData$PrecipPredictor1[i] + OutsideData$PrecipPredictor1[i-2] + OutsideData$PrecipPredictor1[i-1] + OutsideData$PrecipPredictor1[i+1]))) * PrecipPredictor1_Mult) +
                              ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor2[i] / (OutsideData$PrecipPredictor2[i] + OutsideData$PrecipPredictor2[i-2] + OutsideData$PrecipPredictor2[i-1] + OutsideData$PrecipPredictor2[i+1]))) * PrecipPredictor2_Mult)
  }

  else if (str_detect(OutsideData$Date[i], pattern = '4$')){

    Template$Mini[i] <- ((OutsideData$Mini[i] * (OutsideData$MiniPredictor1[i] / (OutsideData$MiniPredictor1[i] + OutsideData$MiniPredictor1[i-3] + OutsideData$MiniPredictor1[i-2] + OutsideData$MiniPredictor1[i-1]))) * MiniPredictor1_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor2[i] / (OutsideData$MiniPredictor2[i] + OutsideData$MiniPredictor2[i-3] + OutsideData$MiniPredictor2[i-2] + OutsideData$MiniPredictor2[i-1]))) * MiniPredictor2_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor3[i] / (OutsideData$MiniPredictor3[i] + OutsideData$MiniPredictor3[i-3] + OutsideData$MiniPredictor3[i-2] + OutsideData$MiniPredictor3[i-1]))) * MiniPredictor3_Mult) +
                        ((OutsideData$Mini[i] * (OutsideData$MiniPredictor4[i] / (OutsideData$MiniPredictor4[i] + OutsideData$MiniPredictor4[i-3] + OutsideData$MiniPredictor4[i-2] + OutsideData$MiniPredictor4[i-1]))) * MiniPredictor4_Mult)

    Template$Small[i] <- ((OutsideData$Small[i] * (OutsideData$SmallPredictor1[i] / (OutsideData$SmallPredictor1[i] + OutsideData$SmallPredictor1[i-3] + OutsideData$SmallPredictor1[i-2] + OutsideData$SmallPredictor1[i-1]))) * SmallPredictor1_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor2[i] / (OutsideData$SmallPredictor2[i] + OutsideData$SmallPredictor2[i-3] + OutsideData$SmallPredictor2[i-2] + OutsideData$SmallPredictor2[i-1]))) * SmallPredictor2_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor3[i] / (OutsideData$SmallPredictor3[i] + OutsideData$SmallPredictor3[i-3] + OutsideData$SmallPredictor3[i-2] + OutsideData$SmallPredictor3[i-1]))) * SmallPredictor3_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor4[i] / (OutsideData$SmallPredictor4[i] + OutsideData$SmallPredictor4[i-3] + OutsideData$SmallPredictor4[i-2] + OutsideData$SmallPredictor4[i-1]))) * SmallPredictor4_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor5[i] / (OutsideData$SmallPredictor5[i] + OutsideData$SmallPredictor5[i-3] + OutsideData$SmallPredictor5[i-2] + OutsideData$SmallPredictor5[i-1]))) * SmallPredictor5_Mult) +
                         ((OutsideData$Small[i] * (OutsideData$SmallPredictor6[i] / (OutsideData$SmallPredictor6[i] + OutsideData$SmallPredictor6[i-3] + OutsideData$SmallPredictor6[i-2] + OutsideData$SmallPredictor6[i-1]))) * SmallPredictor6_Mult)

    Template$Medium[i] <- ((OutsideData$Medium[i] * (OutsideData$MediumPredictor1[i] / (OutsideData$MediumPredictor1[i] + OutsideData$MediumPredictor1[i-3] + OutsideData$MediumPredictor1[i-2] + OutsideData$MediumPredictor1[i-1]))) * MediumPredictor1_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor2[i] / (OutsideData$MediumPredictor2[i] + OutsideData$MediumPredictor2[i-3] + OutsideData$MediumPredictor2[i-2] + OutsideData$MediumPredictor2[i-1]))) * MediumPredictor2_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor3[i] / (OutsideData$MediumPredictor3[i] + OutsideData$MediumPredictor3[i-3] + OutsideData$MediumPredictor3[i-2] + OutsideData$MediumPredictor3[i-1]))) * MediumPredictor3_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor4[i] / (OutsideData$MediumPredictor4[i] + OutsideData$MediumPredictor4[i-3] + OutsideData$MediumPredictor4[i-2] + OutsideData$MediumPredictor4[i-1]))) * MediumPredictor4_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor5[i] / (OutsideData$MediumPredictor5[i] + OutsideData$MediumPredictor5[i-3] + OutsideData$MediumPredictor5[i-2] + OutsideData$MediumPredictor5[i-1]))) * MediumPredictor5_Mult) +
                          ((OutsideData$Medium[i] * (OutsideData$MediumPredictor6[i] / (OutsideData$MediumPredictor6[i] + OutsideData$MediumPredictor6[i-3] + OutsideData$MediumPredictor6[i-2] + OutsideData$MediumPredictor6[i-1]))) * MediumPredictor6_Mult)

    Template$Large[i] <- ((OutsideData$Large[i] * (OutsideData$LargePredictor1[i] / (OutsideData$LargePredictor1[i] + OutsideData$LargePredictor1[i-3] + OutsideData$LargePredictor1[i-2] + OutsideData$LargePredictor1[i-1]))) * LargePredictor1_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor2[i] / (OutsideData$LargePredictor2[i] + OutsideData$LargePredictor2[i-3] + OutsideData$LargePredictor2[i-2] + OutsideData$LargePredictor2[i-1]))) * LargePredictor2_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor3[i] / (OutsideData$LargePredictor3[i] + OutsideData$LargePredictor3[i-3] + OutsideData$LargePredictor3[i-2] + OutsideData$LargePredictor3[i-1]))) * LargePredictor3_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor4[i] / (OutsideData$LargePredictor4[i] + OutsideData$LargePredictor4[i-3] + OutsideData$LargePredictor4[i-2] + OutsideData$LargePredictor4[i-1]))) * LargePredictor4_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor5[i] / (OutsideData$LargePredictor5[i] + OutsideData$LargePredictor5[i-3] + OutsideData$LargePredictor5[i-2] + OutsideData$LargePredictor5[i-1]))) * LargePredictor5_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor6[i] / (OutsideData$LargePredictor6[i] + OutsideData$LargePredictor6[i-3] + OutsideData$LargePredictor6[i-2] + OutsideData$LargePredictor6[i-1]))) * LargePredictor6_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor7[i] / (OutsideData$LargePredictor7[i] + OutsideData$LargePredictor7[i-3] + OutsideData$LargePredictor7[i-2] + OutsideData$LargePredictor7[i-1]))) * LargePredictor7_Mult) +
                         ((OutsideData$Large[i] * (OutsideData$LargePredictor8[i] / (OutsideData$LargePredictor8[i] + OutsideData$LargePredictor8[i-3] + OutsideData$LargePredictor8[i-2] + OutsideData$LargePredictor8[i-1]))) * LargePredictor8_Mult)

    Template$DisRepairs[i] <- ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor1[i] / (OutsideData$PrecipPredictor1[i] + OutsideData$PrecipPredictor1[i-3] + OutsideData$PrecipPredictor1[i-2] + OutsideData$PrecipPredictor1[i-1]))) * PrecipPredictor1_Mult) +
                              ((OutsideData$DisRepair[i] * (OutsideData$PrecipPredictor2[i] / (OutsideData$PrecipPredictor2[i] + OutsideData$PrecipPredictor2[i-3] + OutsideData$PrecipPredictor2[i-2] + OutsideData$PrecipPredictor2[i-1]))) * PrecipPredictor2_Mult)
  }

}

# Template %>%
#   group_by(Year) %>%
#   summarize(Mini = sum(Mini),
#             Small = sum(Small),
#             Medium = sum(Medium),
#             Large = sum(Large))

# Pivot the data to a long format with CostClasses going to the 'CostClass' column, and 1-year total costs to 'TotalJobCost_New'
Template <- Template %>%
  mutate(Date = as.yearqtr(as.character(Date), format = "%Y.%q"))

Template <- Template %>%
  pivot_longer(cols = 3:7, names_to = 'CostClass', values_to = 'TotalJobCost')

Template %>%
  filter(Year >= 2000) %>%
  ggplot(aes(x = Date, y = TotalJobCost/1000000000, fill = fct_rev(CostClass))) +
  scale_y_continuous(labels = function(income) format(income, big.mark = ",", scientific = FALSE), breaks = c(seq(0,110, 10)), limits = c(0,110), expand = c(0,1)) +
  # percent_format()) +
  # function(income) format(income, big.mark = ",", scientific = FALSE)) +
  geom_col(color = NA) +
  scale_x_yearqtr(n = length(unique(Template$Year))*4, format = '%Y Q%q') +
  scale_fill_jbrec(discrete = TRUE, palette = 'PrimaryAndSecondary') +
  geom_text(aes(label = scales::comma(round(TotalJobCost/1000000000)), y = TotalJobCost/1000000000),
            position = position_stack(vjust = .5), color = 'white', size = 4) +
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
        axis.text.x = element_text(angle = 90, vjust =0.5))


AddJBRECLogo(logopath = "C:/Users/ikennedy/Downloads/JBRECLogo.jpg", "C:/Users/ikennedy/Downloads/GraphTest.png", width = 24, height = 10, dpi = 300, logoscale = 8)

write.xlsx(Template, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/QuarterlyApprox_2010_Current.xlsx")

