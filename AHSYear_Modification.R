library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)
library(ClusterR)
library(cluster)
library(conflicted)

# Prefer Dplyr over other packages for certain functions
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

# Convert JOBDIY to NA if it was Not Reported ('NR') and Create a 'Count' variable (this will be used later on)
Data <- Data %>%
  mutate(JOBDIY = ifelse(JOBDIY == 'NR', NA, JOBDIY),
         Count = 1) 

Data <- Data %>%
  filter(AHSYEAR >= 2015)

# Read in the Inflation Adjustment Spreadsheet
Inflation <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSProject/InflationRate.xlsx")
# Select the year and rate columns
Inflation <- Inflation[,c(1,3)]

# Define the Years to be included
# Format: c(seq(from, to, by))
Years <- c(seq(2013, 2021, 1))
SurveyYears <- c(seq(2015, 2021, 2))

# For each Year
for (i in SurveyYears) {
  # Create a 'Data_[YEAR]' for each Year in Years (line 40) without a Mini Class
  assign(paste0('Data_', i),
         value = Data %>%
           # Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
           filter(AHSYEAR == i) %>%
           mutate(AHSYEAR_Modified = ifelse(!is.na(JOBWORKYR), JOBWORKYR, JOBCOMPYR)))
}

#Years <- c(2013, 2014, 2015)
#Years <- c(2015, 2016, 2017)
#Years <- c(2017, 2018, 2019)
Years <- c(2019, 2020, 2021)

# For each Year
for (i in Years) {
  
  # Create a 'Data_[YEAR]' for each Year in Years (line 40) without a Mini Class
  assign(paste0('Data_', i),
         value = Data_2021 %>%
           # Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
           filter(AHSYEAR_Modified == i & !is.na(JobCategory) & TENURE == "Owned/Bought" & JobCategory != 'DisRepairs') %>%
           # Amend JOBCOST to be inflation adjusted
           mutate(JOBCOST = JOBCOST * Inflation[Inflation$Year == i,]$Rate,
                  JOBCOST_Weighted = JOBCOST*WEIGHT,
                  # Create a CostClass variable dependent on inflation-adjusted JOBCOST
                  CostClass = case_when(JOBCOST < 10000 ~ "Small",
                                        JOBCOST >= 10000 & JOBCOST < 30000 ~ 'Medium',
                                        JOBCOST >= 30000 ~ 'Large')))
  
  # Assign a doers count to 'Doers_[YEAR] for each year (present in the 'Doers' column)
  assign(paste0("Doers_", i), 
         value = 
           get(paste0("Data_", i)) %>%
           filter(JOBCOST > 0) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           summarize(Doers = round(sum(WEIGHT, na.rm = TRUE))))
  
  # Assign a jobs count to 'Jobs_[YEAR] for each year (present in the 'Jobs' column)
  assign(paste0("Jobs_", i), 
         value = 
           get(paste0("Data_", i)) %>%
           filter(JOBCOST > 0) %>%
           summarize(Jobs = round(sum(WEIGHT, na.rm = TRUE))))
  
}

# Custom function for aggregating total household, doer, and job counts by year  
TotalSummary <- function(Data, Year){
  assign(paste0('Total', Year),
         value = get(paste0('Data_', i)) %>%
           # Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
           #filter(AHSYEAR_Modified == Year & !is.na(JobCategory) & TENURE == "Owned/Bought") %>%
           summarize(Year = Year,
                     CostClass = 'All',
                     # Place the Household counts in the 'Households' column
                     #Households = round(get(paste0("Households_", Year))$Households),
                     # Place the Doer counts in the 'Doers' column
                     Doers = get(paste0("Doers_", Year))$Doers,
                     # Place the Job counts in the 'Jobs' column
                     Jobs = get(paste0("Jobs_", Year))$Jobs,
                     # Create 'TotalJobCost' by multiplying the given year's inflation rate by the sum of the weighted JOBCOST
                     TotalJobCost =  sum(JOBCOST_Weighted, na.rm = TRUE),
                     # Create 'JobsPerDoer' by divding Jobs by Doers
                     JobsPerDoer = Jobs/Doers,
                     # Create 'MeanCost' by multiplying the given year's inflation rate by the TotalJobCost/Jobs
                     MeanCost = TotalJobCost/Jobs,
                     # Create 'MeanCostPerHousehold' by multiplying the given year's inflation rate by the TotalJobCost/Households
                     #MeanCostPerHousehold = Inflation[Inflation$Year == i,]$Rate * (TotalJobCost/Households),
                     # Create 'MeanCostPerDoer' by multiplying the given year's inflation rate by the TotalJobCost/Doers
                     MeanCostPerDoer = (TotalJobCost/Doers)))
  
}

# For each Year
for (i in Years) {
  # Create 'Total_[YEAR]', by using the custom function above on each Data_[YEAR]
  assign(paste0('Total_', i),
         value = TotalSummary(Data, i))
  
  # Create 'Design_[YEAR]', by using the appropriate 'Data_[YEAR] object
  # assign(paste0('Design_', i),
  #        value = svydesign(data = get(paste0("Data_", i)), id = ~1, weights = get(paste0("Data_", i))$WEIGHT))
}

Final_AHS_2015 <- Total_2013 %>%
  rbind(Total_2014, Total_2015)

Final_AHS_2017 <- Total_2015 %>%
  rbind(Total_2016, Total_2017)

Final_AHS_2019 <- Total_2017 %>%
  rbind(Total_2018, Total_2019)

Final_AHS_2021 <- Total_2019 %>%
  rbind(Total_2020, Total_2021)

Final <- Final_AHS_2015 %>%
  rbind(Final_AHS_2017, Final_AHS_2019, Final_AHS_2021)

RemoveList <- ls()
RemoveList <- RemoveList[-c(1, 20:24)]
rm(list = RemoveList)


FinalCondensed <- Final %>%
  group_by(Year) %>%
  summarize(CostClass = 'All',
            Doers = sum(Doers),
            Jobs = sum(Jobs),
            TotalJobCost = sum(TotalJobCost),
            JobsPerDoer = Jobs/Doers, 
            MeanCost = mean(MeanCost),
            MeanCostPerDoer = TotalJobCost/Doers)

TotalCost <- FinalCondensed %>%
  summarize('2015' = TotalJobCost[1] + TotalJobCost[2] + .67*TotalJobCost[3],
         '2017' = .33*TotalJobCost[3] + TotalJobCost[4] + .67*TotalJobCost[5],
         '2019' = .33*TotalJobCost[5] + TotalJobCost[6] + .67*TotalJobCost[7],
         '2021' = .33*TotalJobCost[7] + TotalJobCost[8] + TotalJobCost[9])
TotalCost <- TotalCost %>%
  pivot_longer(names_to = 'Year', values_to = 'TotalJobCost', cols = c('2015', '2017', '2019', '2021'))

Jobs <- FinalCondensed %>%
  summarize('2015' = Jobs[1] + Jobs[2] + .67*Jobs[3],
            '2017' = .33*Jobs[3] + Jobs[4] + .67*Jobs[5],
            '2019' = .33*Jobs[5] + Jobs[6] + .67*Jobs[7],
            '2021' = .33*Jobs[7] + Jobs[8] + Jobs[9])

Jobs <- Jobs %>%
  pivot_longer(names_to = 'Year', values_to = 'Jobs', cols = c('2015', '2017', '2019', '2021'))

Doers <- FinalCondensed %>%
  summarize('2015' = Doers[1] + Doers[2] + .67*Doers[3],
            '2017' = .33*Doers[3] + Doers[4] + .67*Doers[5],
            '2019' = .33*Doers[5] + Doers[6] + .67*Doers[7],
            '2021' = .33*Doers[7] + Doers[8] + Doers[9])

Doers <- Doers %>%
  pivot_longer(names_to = 'Year', values_to = 'Doers', cols = c('2015', '2017', '2019', '2021'))

FinalCondensed_Rolling <- TotalCost %>%
  left_join(Jobs, by = 'Year') %>%
  left_join(Doers, by = 'Year') %>%
  mutate(JobsPerDoer = Jobs/Doers,
         MeanCost = TotalJobCost/Jobs,
         MeanCostPerDoer = TotalJobCost/Doers)
         
dataset_names <- list('2015' = Final_AHS_2015, '2017' = Final_AHS_2017, '2019' = Final_AHS_2019, '2021' = Final_AHS_2021, 
                      'AllYears' = Final, 'AllYears_Condensed' = FinalCondensed, 'AllYears_Condensed_Rolling' = FinalCondensed_Rolling)


write.xlsx(dataset_names, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSPanel_SingleYear.xlsx")
