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

# Read in the Raw AHS Data
Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

# Convert JOBDIY to NA if it was Not Reported ('NR') and Create a 'Count' variable (this will be used later on)
Data <- Data %>%
  mutate(JOBDIY = ifelse(JOBDIY == 'NR', NA, JOBDIY),
         Count = 1)

# Read in the Inflation Adjustment Spreadsheet
Inflation <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSProject/InflationRate.xlsx")
# Select the year and rate columns
Inflation <- Inflation[,c(1,3)]

# Read in the 1995 data
Households_1995_Amended <- read.csv("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSProject/1995/ahs1995n.csv") %>%
  # Drop all duplicated CONTROLs (i.e. households)
  distinct(CONTROL, .keep_all = TRUE) %>%
  # Filter for 'Owned/Bought' ("1" here)
  filter(TENURE %in% c("'1'")) %>%
  # Calculate the number of Households for 1995
  summarize(Households = sum(WEIGHT))
# Transer the 1995 household count to Households_1995_Amended
Households_1995_Amended <- round(Households_1995_Amended$Households)

# Define the Years to be included
# Format: c(seq(from, to, by))
Years <- c(seq(1995, 2021, 2))


# For each Year
for (i in Years) {
  # Create a 'Data_[YEAR]' for each Year in Years (line 40) without a Mini Class
  assign(paste0('Data_', i),
         value = Data %>%
           # Filter for data equal to the year in question, with a job category that is not 'DisRepairs', and that is 'Owned/Bought'
           filter(AHSYEAR == i & !is.na(JobCategory) & TENURE == "Owned/Bought" & JobCategory != 'DisRepairs') %>%
           # Amend JOBCOST to be inflation adjusted
           mutate(JOBCOST = JOBCOST * Inflation[Inflation$Year == i,]$Rate,
                  # Create a CostClass variable dependent on inflation-adjusted JOBCOST
                  CostClass = case_when(JOBCOST < 10000 ~ "Small",
                                        JOBCOST >= 10000 & JOBCOST < 30000 ~ 'Medium',
                                        JOBCOST >= 30000 ~ 'Large')))
}

# For each Year
# for (i in Years) {
# Create a 'Data_[YEAR]' for each Year in Years (line 40) with a Mini Class
#   assign(paste0('Data_', i),
#          value = Data %>%
# Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
#            filter(AHSYEAR == i & !is.na(JobCategory) & TENURE == "Owned/Bought" & JobCategory != 'DisRepairs') %>%
# Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
#            mutate(JOBCOST = JOBCOST * Inflation[Inflation$Year == i,]$Rate,
# Create a CostClass variable dependent on inflation-adjusted JOBCOST
#                   CostClass = case_when(JOBCOST < 1800 ~ "Mini",
#                                         JOBCOST >= 1800 & JOBCOST < 10000 ~ "Small",
#                                         JOBCOST >= 10000 & JOBCOST < 30000 ~ 'Medium',
#                                         JOBCOST >= 30000 ~ 'Large')))
# }

# For each Year
for (i in Years) {
  # Assign a household count to 'Households_[YEAR] for each year (present in the 'Households' column)
  assign(paste0("Households_", i), 
         value = Data %>% 
           filter(AHSYEAR == i & TENURE == "Owned/Bought") %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           summarize(Households = sum(WEIGHT, na.rm = TRUE)))
  
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
         value = Data %>%
           # Filter for data equal to the year in question, with a job category, and that is 'Owned/Bought'
           filter(AHSYEAR == Year & !is.na(JobCategory) & TENURE == "Owned/Bought" & JobCategory != 'DisRepairs') %>%
           summarize(Year = Year,
                     CostClass = 'All',
                     # Place the Household counts in the 'Households' column
                     Households = round(get(paste0("Households_", Year))$Households),
                     # Place the Doer counts in the 'Doers' column
                     Doers = get(paste0("Doers_", Year))$Doers,
                     # Place the Job counts in the 'Jobs' column
                     Jobs = get(paste0("Jobs_", Year))$Jobs,
                     # Create 'TotalJobCost' by multiplying the given year's inflation rate by the sum of the weighted JOBCOST
                     TotalJobCost =  Inflation[Inflation$Year == i,]$Rate * sum(WEIGHT*JOBCOST, na.rm = TRUE),
                     # Create 'JobsPerDoer' by divding Jobs by Doers
                     JobsPerDoer = Jobs/Doers,
                     # Create 'MeanCost' by multiplying the given year's inflation rate by the TotalJobCost/Jobs
                     MeanCost = Inflation[Inflation$Year == i,]$Rate * (TotalJobCost/Jobs),
                     # Create 'MeanCostPerHousehold' by multiplying the given year's inflation rate by the TotalJobCost/Households
                     MeanCostPerHousehold = Inflation[Inflation$Year == i,]$Rate * (TotalJobCost/Households),
                     # Create 'MeanCostPerDoer' by multiplying the given year's inflation rate by the TotalJobCost/Doers
                     MeanCostPerDoer = Inflation[Inflation$Year == i,]$Rate * (TotalJobCost/Doers)))
  
}

# For each Year
for (i in Years) {
  # Create 'Total_[YEAR]', by using the custom function above on each Data_[YEAR]
  assign(paste0('Total_', i),
         value = TotalSummary(Data, i))
  
  # Create 'Design_[YEAR]', by using the appropriate 'Data_[YEAR] object
  assign(paste0('Design_', i),
         value = svydesign(data = get(paste0("Data_", i)), id = ~1, weights = get(paste0("Data_", i))$WEIGHT))
}


# For each Year
for (i in Years) {
  # Create 'CostClassDoers_[YEAR]'
  assign(paste0("CostClassDoers_", i), 
         value = 
           # Use the appropriate 'Data_[YEAR] object
           get(paste0('Data_', i)) %>% 
           # Filter for JOBCOSTs > 0
           filter(JOBCOST > 0) %>%
           # Drop all duplicated CONTROLs (i.e. households)
           distinct(CONTROL, .keep_all = TRUE) %>%
           # Group by 'CostClass'
           group_by(CostClass) %>%
           # Find the number of Doers in each 'CostClass' (ignoring NA values) by summing the unique Weights 
           # (without the 'distinct' call this number would mirror Jobs)
           summarize(Doers = round(sum(WEIGHT, na.rm = TRUE))))
  
  # Create 'CostClassJobs_[YEAR]'
  assign(paste0("CostClassJobs_", i), 
         value = 
           # Use the appropriate 'Data_[YEAR] object
           get(paste0('Data_', i)) %>% 
           # Filter for JOBCOSTs > 0
           filter(JOBCOST > 0) %>%
           # Group by 'CostClass'
           group_by(CostClass) %>%
           # Find the number of Jobs in each 'CostClass' (ignoring NA values) by summing the Weights 
           summarize(Jobs = round(sum(WEIGHT, na.rm = TRUE))))
  
  # Create 'CostClassTotals_[YEAR]'
  assign(paste0('CostClassTotals_', i),
         # Find the Total JOBCOST by CostClass (weighted), by using the appropriate 'Design_[YEAR] 
         value = svyby(~JOBCOST, ~CostClass, design = get(paste0("Design_", i)), svytotal, na.rm = TRUE))
  
  # Create 'CostClassMeans_[YEAR]'
  assign(paste0('CostClassMeans_', i),
         # Find the Mean JOBCOST by CostClass (weighted), by using the appropriate 'Design_[YEAR]
         value = svyby(~JOBCOST, ~CostClass, design = get(paste0("Design_", i)), svymean, na.rm = TRUE))
  
}

# For each Year
for (i in Years) {
  # Create 'CostClassTotal_[YEAR]', grabbing values from the various dataframes that had been created previously. 
  assign(paste0("CostClassTotal_", i),
         # Create a dataframe
         value = data.frame(Year = i,
                            CostClass = get(paste0("CostClassDoers_", i))$CostClass,
                            Doers = get(paste0("CostClassDoers_", i))$Doers,
                            Jobs = get(paste0("CostClassJobs_", i))$Jobs,
                            TotalJobCost = get(paste0("CostClassTotals_", i))$JOBCOST) %>%
           mutate(JobsPerDoer = Jobs/Doers,
                  MeanCost = get(paste0("CostClassMeans_", i))$JOBCOST,
                  MeanCostPerHousehold = NA,
                  MeanCostPerDoer = TotalJobCost/Doers))
  
  
  assign(paste0("Total_", i),
         value = get(paste0("Total_", i)) %>%
           plyr::rbind.fill(get(paste0("CostClassTotal_", i))) %>%
           mutate(Households = round(get(paste0("Households_", i))$Households)) %>%
           mutate(MeanCostPerHousehold = TotalJobCost/Households))
  
}

# All calculations up to this point do not include Disaster Repair projects, the next section calculates values for those projects seperately
# For each year
for (i in Years){
  
  # Create a 'DisRepairsData_[YEAR], containing data pertaining only to Disaster Repairs
  assign(paste0("DisRepairsData_", i), 
         value = Data %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought" & JobCategory == 'DisRepairs'))
  
  # Assign a doers count to 'DisRepairsDoers_[YEAR]
  assign(paste0("DisRepairsDoers_", i), 
         value = get(paste0('DisRepairsData_', i)) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           summarize(Doers = sum(WEIGHT)))
  # Modify 'DisRepairsDoers_[YEAR] to be a numeric value
  assign(paste0("DisRepairsDoers_", i), 
         value = round(as.numeric(get(paste0('DisRepairsDoers_', i))$Doers)))
  
  # Assign a jobs count to 'DisRepairsJobs_[YEAR]
  assign(paste0("DisRepairsJobs_", i), 
         value = get(paste0('DisRepairsData_', i)) %>%
           filter(JOBCOST > 0) %>%
           summarize(Jobs = sum(WEIGHT)))
  # Modify 'DisRepairsJobs_[YEAR] to be a numeric value
  assign(paste0("DisRepairsJobs_", i), 
         value = round(as.numeric(get(paste0('DisRepairsJobs_', i))$Jobs)))
  
  # Assign a Total Cost calculation to 'DisRepairsCost_[YEAR]
  assign(paste0("DisRepairsCost_", i), 
         value = get(paste0('DisRepairsData_', i)) %>%
           # Create 'JOBCOST_Weighted' by weighting 'JOBCOST'
           mutate(JOBCOST_Weighted = JOBCOST*WEIGHT) %>%
           # Find the total job cost by summing 'JOBCOST_Weighted'
           summarize(TotalJobCost = sum(JOBCOST_Weighted)))
  # Modify 'DisRepairsCost_[YEAR] to be a numeric value
  assign(paste0("DisRepairsCost_", i), 
         value = as.numeric(get(paste0('DisRepairsCost_', i))$TotalJobCost))
  
  # Assign a Mean Cost calculation to 'DisRepairsMeanCost_[YEAR]
  assign(paste0("DisRepairsMeanCost_", i), 
         value = get(paste0('DisRepairsData_', i)) %>%
           summarize(MeanJobCost = mean(JOBCOST)))
  # Modify 'DisRepairsMeanCost_[YEAR] to be a numeric value
  assign(paste0("DisRepairsMeanCost_", i), 
         value = as.numeric(get(paste0('DisRepairsMeanCost_', i))$MeanJobCost))
  
  # Create 'DisRepairs_[YEAR]', containing all relevant data for Disaster Repair projects. These columns mirror columns present in 'Total_[YEAR]' (where non Disaster Repair data lies at this point)
  assign(paste0("DisRepairs_", i), 
         value = data.frame(Year = i, 
                            CostClass = 'DisRepairs', 
                            Households = round(get(paste0('Households_', i))$Households), 
                            Doers = get(paste0('DisRepairsDoers_', i)), 
                            Jobs = get(paste0('DisRepairsJobs_', i)), 
                            TotalJobCost = get(paste0('DisRepairsCost_', i)), 
                            JobsPerDoer = get(paste0('DisRepairsJobs_', i))/get(paste0('DisRepairsDoers_', i)), 
                            MeanCost = get(paste0('DisRepairsMeanCost_', i)),
                            MeanCostPerDoer = get(paste0('DisRepairsCost_', i))/get(paste0('DisRepairsDoers_', i))) %>%
           mutate(MeanCostPerHousehold = TotalJobCost/Households))
  
  # Bind 'DisRepairs_[YEAR]' to 'Total_[YEAR]' by row
  assign(paste0("Total_", i),
         value = get(paste0("Total_", i)) %>%
           rbind(get(paste0("DisRepairs_", i))))
  
}

# Bind all 'Total_[YEAR]' datasets together and save the final AHS Panel as 'Final'
Final <- Total_1995 %>%
  rbind(Total_1997, Total_1999, Total_2001, Total_2003, Total_2005, Total_2007, Total_2009, 
        Total_2011, Total_2013, Total_2015, Total_2017, Total_2019, Total_2021) %>%
  # Amend the 1995 household count and cost per household
  mutate(Households = ifelse(Year == 1995, Households_1995_Amended, Households),
         MeanCostPerHousehold = ifelse(Year == 1995, TotalJobCost/Households, MeanCostPerHousehold))

# Output the final AHS Panel
write.xlsx(Final, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSPanel_7.24.23.xlsx")