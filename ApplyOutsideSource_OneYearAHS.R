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
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

Correlations <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/AllCorrelations_9.6.xlsx")

Correlations <- Correlations %>%
  select(Variable, Mini, Small, Medium, Large, DisRepair)

# Read in Consumer Expenditure (CES) and Retail Trade Survey Data (ARTS) and convert ARTS's year column to numeric
CES <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 1)
ARTS <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/DataSourcesCondensed.xlsx", sheet = 2)
ARTS$Year <- as.numeric(ARTS$Year)
# Read in NE Precipitation Data and select the column of interest (Apr here performed best for Disaster Repair spending)
NEPrecip <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/USNortheast_TotalPrecipitation.xlsx")
NEPrecip <- NEPrecip %>%
  select(Year, Apr)

# Filter each outside dataset for years 1994-current
CES <- CES %>%
  filter(Year >= 1994)
ARTS <- ARTS %>%
  filter(Year >= 1994)


# Read in the Inflation Adjustment Spreadsheet
Inflation <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/InflationRate.xlsx")
# Select the year and rate columns
Inflation <- Inflation[,c(1,3)]

# Join the Inflation data to the ARTS data by year.
ARTS <- ARTS %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(Inflation, by = 'Year')
# Inflation adjust all ARTS columns (aside from year of course)
ARTS[2:21] <- ARTS[2:21] * ARTS$Rate
# Driop==op the inflation rate column
ARTS <- ARTS %>%
  select(-Rate)

# Join the Inflation data to the CES data by year
CES <- CES %>%
  left_join(Inflation, by = 'Year')
# Inflation adjust all CES columns
CES[2:29] <- CES[2:29] * CES$Rate
# Drop the inflation rate column
CES <- CES %>%
  select(-Rate)

# Remove the Inflation data
rm(Inflation)

# Bind together the ARTS and CES data by column (1 column for each variable)
OutsideSourcesCondensed <- CES %>%
  cbind(ARTS[,-1])
# Join the NE Precipitation data to the ARTS/CES data by year
OutsideSourcesCondensed <- OutsideSourcesCondensed %>%
  left_join(NEPrecip, by = 'Year')

# Select the columns to be used for 1-year breakouts
OutsideSourcesCondensed <- OutsideSourcesCondensed %>%
  select(Year, MajorAppliances_40, MajorAppliances_80, RetailTotal_NoCar, BuildingMat_SuppliesDealers, FuelDealers, Apr)

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

# Create a 'MiniPredictor' column in the template dataframe containing the variable to use for Mini (MajorAppliances_40), then join the 2-Year Mini data to the template by year
## Do the same for each of the other cost classes. For Disaster Repairs paste both of the predictor columns
Template$MiniPredictor <- OutsideSourcesCondensed$MajorAppliances_40
Template <- Template %>%
  left_join(Mini, by = 'Year')

Template$SmallPredictor <- OutsideSourcesCondensed$MajorAppliances_80
Template <- Template %>%
  left_join(Small, by = 'Year')

Template$MediumPredictor <- OutsideSourcesCondensed$RetailTotal_NoCar
Template <- Template %>%
  left_join(Medium, by = 'Year')

Template$LargePredictor <- OutsideSourcesCondensed$BuildingMat_SuppliesDealers
Template <- Template %>%
  left_join(Large, by = 'Year')

Template$FuelDealers <- OutsideSourcesCondensed$FuelDealers
Template$Precip <- OutsideSourcesCondensed$Apr
Template <- Template %>%
  left_join(DisRepairs, by = 'Year')

# Establish the multiplier to use for FuelDealers/Apr (i.e. Fuel proxy/April NE Precipitation)
FDMult <- 0.572212466
PrecipMult <- 0.427787534

# For each year from 1994-current
for (i in c(seq(1994,2021,1))){
  # If the year is even (i.e. an not an AHS Survey YEar)
  if (i %% 2 == 0){
    # Find the proportion of the MiniPredictor that falls within each of the bordering years (i.e. proportion of MiniPredictor that falls in 1994 vs 1995, a fraction)
    ## Multiply the proportion by the 2-Year total to find 1-year total for the given year. Paste that value into the 'Mini' column
    Template[Template$Year == i, ]$Mini <-Template[Template$Year == i+1, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor/(Template[Template$Year == i, ]$MiniPredictor + Template[Template$Year == i+1, ]$MiniPredictor))
    # Do the same for other CostClasses when an even year
    Template[Template$Year == i, ]$Small <- Template[Template$Year == i+1, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor/(Template[Template$Year == i, ]$SmallPredictor + Template[Template$Year == i+1, ]$SmallPredictor))
    Template[Template$Year == i, ]$Medium <- Template[Template$Year == i+1, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor/(Template[Template$Year == i, ]$MediumPredictor + Template[Template$Year == i+1, ]$MediumPredictor))
    Template[Template$Year == i, ]$Large <- Template[Template$Year == i+1, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor/(Template[Template$Year == i, ]$LargePredictor + Template[Template$Year == i+1, ]$LargePredictor))
    
    # Do the same for each of the disaster repair predictor columns, though use the multipliers to find the appropriate value (i.e. FuelDealers 1-Year Total X FDMult + NEPrecip 1-Year Total X PrecipMult)
    Template[Template$Year == i, ]$DisRepairs <- 
      ((Template[Template$Year == i+1, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$FuelDealers/(Template[Template$Year == i, ]$FuelDealers + Template[Template$Year == i+1, ]$FuelDealers)) * FDMult)) +
      ((Template[Template$Year == i+1, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$Precip/(Template[Template$Year == i, ]$Precip + Template[Template$Year == i+1, ]$Precip))) * PrecipMult)
  }
  # If the year is odd
  else{
    # Find the predictor-proportioned values by using the prior year rather than the year ahead (i.e. if i = 1995, we want to use 1994 vs 1995 not 1995 vs 1996)
    Template[Template$Year == i, ]$Mini <- Template[Template$Year == i, ]$TotalJobCost_Mini * (Template[Template$Year == i, ]$MiniPredictor/(Template[Template$Year == i, ]$MiniPredictor + Template[Template$Year == i-1, ]$MiniPredictor))
    Template[Template$Year == i, ]$Small <- Template[Template$Year == i, ]$TotalJobCost_Small * (Template[Template$Year == i, ]$SmallPredictor/(Template[Template$Year == i, ]$SmallPredictor + Template[Template$Year == i-1, ]$SmallPredictor))
    Template[Template$Year == i, ]$Medium <- Template[Template$Year == i, ]$TotalJobCost_Medium * (Template[Template$Year == i, ]$MediumPredictor/(Template[Template$Year == i, ]$MediumPredictor + Template[Template$Year == i-1, ]$MediumPredictor))
    Template[Template$Year == i, ]$Large <- Template[Template$Year == i, ]$TotalJobCost_Large * (Template[Template$Year == i, ]$LargePredictor/(Template[Template$Year == i, ]$LargePredictor + Template[Template$Year == i-1, ]$LargePredictor))
    
    # Do the same for each of the disaster repair predictor columns, though use the multipliers to find the appropriate value (i.e. FuelDealers 1-Year Total X FDMult + NEPrecip 1-Year Total X PrecipMult)
    Template[Template$Year == i, ]$DisRepairs <- 
      (Template[Template$Year == i, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$FuelDealers/(Template[Template$Year == i, ]$FuelDealers + Template[Template$Year == i-1, ]$FuelDealers)) * FDMult) +
      (Template[Template$Year == i, ]$TotalJobCost_DisRepairs * (Template[Template$Year == i, ]$Precip/(Template[Template$Year == i, ]$Precip + Template[Template$Year == i-1, ]$Precip)) * PrecipMult)
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



Template %>%
  ggplot(aes(x = Year, y = TotalJobCost_New/1000000000, fill = fct_rev(CostClass))) +
  scale_y_continuous(labels = function(income) format(income, big.mark = ",", scientific = FALSE), breaks = c(seq(0,600,50)), limits = c(0,600)) +
  # percent_format()) +
  # function(income) format(income, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = c(seq(1994, 2021, 1))) +
  geom_col(color = NA) +
  scale_fill_jbrec(discrete = TRUE, palette = 'PrimaryAndSecondary') +
  geom_text(aes(label = scales::comma(round(TotalJobCost_New/1000000000)), y = TotalJobCost_New/1000000000),
            position = position_stack(vjust = .5), color = 'black', size = 4) +
  labs(
    x = "Year", 
    y = "TotalSpend", 
    title = "Total Spend by Project Type",
    caption = "Source: John Burns Research and Consulting", 
    subtitle = 'Billions of USD (Real)'
  ) +
  theme_jbrec(textcolor = 'black', backgroundcolor = 'white') +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 90)) 

  facet_wrap(~CostClass, scales = 'free', nrow = 4)

write.xlsx(Template, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/OneYearApprox.xlsx")
