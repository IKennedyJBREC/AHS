library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)
library(conflicted)

# Prefer Dplyr over other packages for certain functions
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

# Read in the Raw AHS Data
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/CleanedData/Data_1995_2021.xlsx")

# Convert JOBDIY to NA if it was Not Reported ('NR') and Create a 'Count' variable (this will be used later on)
Data <- Data %>%
  mutate(JOBDIY = ifelse(JOBDIY == 'NR', NA, JOBDIY),
         Count = 1)

Reweights <- read.csv("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSGeneralInfo/2013 AHS_Harvard JCHS reweights.csv")
Reweights <- Reweights %>%
  rename(CONTROL = control) %>%
  select(CONTROL, jchsweight2)
Reweights$CONTROL <- as.character(Reweights$CONTROL)
Data_2013 <- Data %>%
  filter(AHSYEAR == 2013)

Data_2013 <- Data_2013 %>%
  left_join(Reweights, by = 'CONTROL')

Data_2013 <- Data_2013 %>%
  mutate(WEIGHT = jchsweight2) %>%
  select(-jchsweight2)

Data <- Data %>%
  filter(AHSYEAR != 2013)

Data <- Data %>%
  rbind(Data_2013) %>%
  arrange(AHSYEAR)

# Read in the Inflation Adjustment Spreadsheet
Inflation <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/InflationRate.xlsx")
# Select the year and rate columns
Inflation <- Inflation[,c(1,3)]

# Read in the 1995 data
Households_1995_Amended <- read.csv("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/1995/ahs1995n.csv") %>%
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
                  CostClass = case_when(JOBCOST <= 1800 ~ 'Mini',
                                        JOBCOST > 1800 & JOBCOST <= 10000 ~ "Small",
                                        JOBCOST > 10000 & JOBCOST <= 30000 ~ 'Medium',
                                        JOBCOST > 30000 ~ 'Large')))
}

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
                     TotalJobCost =  Inflation[Inflation$Year == i,]$Rate * sum(WEIGHT*JOBCOST, na.rm = TRUE)))
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
                            TotalJobCost = get(paste0("CostClassTotals_", i))$JOBCOST))
  
  assign(paste0("Total_", i),
         value = get(paste0("Total_", i)) %>%
           plyr::rbind.fill(get(paste0("CostClassTotal_", i))) %>%
           mutate(Households = round(get(paste0("Households_", i))$Households))) 
  
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
  
  # Create 'DisRepairs_[YEAR]', containing all relevant data for Disaster Repair projects. These columns mirror columns present in 'Total_[YEAR]' (where non Disaster Repair data lies at this point)
  assign(paste0("DisRepairs_", i), 
         value = data.frame(Year = i, 
                            CostClass = 'DisRepairs', 
                            Households = round(get(paste0('Households_', i))$Households), 
                            Doers = get(paste0('DisRepairsDoers_', i)), 
                            Jobs = get(paste0('DisRepairsJobs_', i)), 
                            TotalJobCost = get(paste0('DisRepairsCost_', i))))
  
  # Bind 'DisRepairs_[YEAR]' to 'Total_[YEAR]' by row
  assign(paste0("Total_", i),
         value = get(paste0("Total_", i)) %>%
           rbind(get(paste0("DisRepairs_", i))))
  
}

# Read in the Raw AHS Data
Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/CleanedData/Data_1995_2021.xlsx")

Data <- Data %>%
  mutate(MAINTAMT = as.numeric(MAINTAMT)) %>%
  filter(MAINTAMT >= 0) %>%
  mutate(JobCategory = ifelse(!is.na(MAINTAMT), 'Maintenance', NA),
         JOBCOSTWeighted = WEIGHT*MAINTAMT) 

Data <- Data %>%
  mutate(JobCategory = ifelse(MAINTAMT <= 1800, 'MaintMini', 'MaintSmall'))

# All calculations up to this point do not include Maintanence projects, the next section calculates values for those projects seperately
# For each year
for (i in Years){
  
  # Create a 'MaintenanceData_[YEAR], containing data pertaining only to Disaster Repairs
  assign(paste0("MaintenanceData_", i), 
         value = Data %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought"))

  # Assign a doers count to 'MaintenanceDoers_[YEAR]
  assign(paste0("MaintenanceDoers_", i), 
         value = get(paste0('MaintenanceData_', i)) %>%
           filter(!is.na(MAINTAMT)) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           group_by(JobCategory) %>%
           summarize(Doers = sum(WEIGHT)))
  # Modify 'MaintenanceDoers_[YEAR] to be a numeric value
  assign(paste0("MaintenanceDoers_", i), 
         value = round(as.numeric(get(paste0('MaintenanceDoers_', i))$Doers)))
  
  # Assign a jobs count to 'MaintenanceJobs_[YEAR]
  assign(paste0("MaintenanceJobs_", i), 
         value = get(paste0('MaintenanceData_', i)) %>%
           filter(!is.na(MAINTAMT)) %>%
           group_by(JobCategory) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           summarize(Jobs = sum(WEIGHT)))
  # Modify 'MaintenanceJobs_[YEAR] to be a numeric value
  assign(paste0("MaintenanceJobs_", i), 
         value = round(as.numeric(get(paste0('MaintenanceJobs_', i))$Jobs)))
 
  # Assign a Total Cost calculation to 'MaintenanceCost_[YEAR]
  assign(paste0("MaintenanceCost_", i), 
         value = get(paste0('MaintenanceData_', i)) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           group_by(JobCategory) %>%
           # Find the total job cost by summing 'JOBCOST_Weighted'
           summarize(TotalJobCost = sum(JOBCOSTWeighted)))
  # Modify 'MaintenanceCost_[YEAR] to be a numeric value
  assign(paste0("MaintenanceCost_", i), 
         value = as.numeric(get(paste0('MaintenanceCost_', i))$TotalJobCost))

  # Create 'Maintenance_[YEAR]', containing all relevant data for Disaster Repair projects. These columns mirror columns present in 'Total_[YEAR]' (where non Disaster Repair data lies at this point)
  assign(paste0("Maintenance_", i), 
         value = data.frame(Year = i, 
                            CostClass = 'Maintenance', 
                            Households = round(get(paste0('Households_', i))$Households), 
                            Doers = get(paste0('MaintenanceDoers_', i)), 
                            Jobs = get(paste0('MaintenanceJobs_', i)), 
                            TotalJobCost = get(paste0('MaintenanceCost_', i))))
  
  # Bind 'Maintenance_[YEAR]' to 'Total_[YEAR]' by row
  assign(paste0("Total_", i),
         value = get(paste0("Total_", i)) %>%
           rbind(get(paste0("Maintenance_", i))))
  
}

# Bind all 'Total_[YEAR]' datasets together and save the final AHS Panel as 'Final'
Final <- Total_1995 %>%
  rbind(Total_1997, Total_1999, Total_2001, Total_2003, Total_2005, Total_2007, Total_2009, 
        Total_2011, Total_2013, Total_2015, Total_2017, Total_2019, Total_2021) %>%
  # Amend the 1995 household count and cost per household
  mutate(Households = ifelse(Year == 1995, Households_1995_Amended, Households))

# Join the inflation data by year
Final <- Final %>%
  left_join(Inflation, by = 'Year') 
# Inflation adjust only Maintenance TotalJobCosts, others are already inflation adjusted, and then drop the inflation rate column
Final <- Final %>%
  mutate(TotalJobCost = ifelse(CostClass == 'Maintenance', TotalJobCost*Rate, TotalJobCost)) %>%
  select(-Rate)

# Create 'Maintenance' containing all Maintenance values. Both Mini/Small Maintenance rows are coded with CostClass = 'Maintenance'. They need to be edited to read 'Maintenance_Mini'/'Maintenance_Small'
Maintenance <- Final %>%
  filter(CostClass == 'Maintenance')

# Pull out even rows (i.e. Small Maintenance info)
SmallMaint <- Maintenance[seq(2, nrow(Maintenance), by = 2), ]
# Pull out odd rows (i.e. Mini Maintenance info)
MiniMaint <- Maintenance[seq(1, nrow(Maintenance), by = 2), ]

# Amend the CostClass labels for each group of Maintenance info
SmallMaint <- SmallMaint %>%
  mutate(CostClass = 'Maintenance_Small',
         TotalJobCost = TotalJobCost*2,
         Jobs = Jobs*2,
         Doers = Doers*2)
# Amend the CostClass labels for each group of Maintenance info
MiniMaint <- MiniMaint %>%
  mutate(CostClass = 'Maintenance_Mini',
         TotalJobCost = TotalJobCost*2,
         Jobs = Jobs*2,
         Doers = Doers*2)

# Filter the final summary for non-Maintenance then reattach the Mini/Smalll Maintenance information
Final <- Final %>%
  filter(CostClass %in% c('Mini', 'Small', 'Medium', 'Large', 'DisRepairs')) %>%
  rbind(SmallMaint) %>%
  rbind(MiniMaint)

# Store yearly household counts in 'Households'
Households <- Final %>%
  filter(CostClass == 'Large') %>%
  select(Year, Households)

# Create 'Mini', containing all mini jobs (maintenance included)
## Change the 'CostClass' label to 'Mini' and find the total Doers/Jobs/Spend. Then add a household variable and reorder the columns.
Mini <- Final %>%
  filter(CostClass %in% c('Mini', 'Maintenance_Mini')) %>%
  group_by(Year) %>%
  summarize(CostClass = 'Mini',
            #Households = Households,
            Doers = sum(Doers),
            Jobs = sum(Jobs),
            TotalJobCost = sum(TotalJobCost)) %>%
  left_join(Households, by = 'Year')
Mini <- Mini[, c(1, 6, 2:5)]

# Create 'Small', containing all small jobs (maintenance included)
## Change the 'CostClass' label to 'Small' and find the total Doers/Jobs/Spend. Then add a household variable and reorder the columns.
Small <- Final %>%
  filter(CostClass %in% c('Small', 'Maintenance_Small')) %>%
  group_by(Year) %>%
  summarize(CostClass = 'Small',
            #Households = Households,
            Doers = sum(Doers),
            Jobs = sum(Jobs),
            TotalJobCost = sum(TotalJobCost)) %>%
  left_join(Households, by = 'Year')
Small <- Small[, c(1, 6, 2:5)]

# Filter the Final summary for only Medium/Large/Disaster Repair data
Final <- Final %>%
  filter(CostClass %in% c('Medium', 'Large', 'DisRepairs')) 

# Reattach the condensed Mini/Small data (now with maintenance included)
Final <- Final %>%
  rbind(Mini) %>%
  rbind(Small) %>%
  arrange(Year, CostClass)

# Create 'ZeroMaint', storing all MAINTAMT responses that were equal to 0 (i.e. no annual maintenance costs)
ZeroMaint <- Data %>%
  group_by(AHSYEAR, JobCategory) %>%
  filter(MAINTAMT == 0) %>%
  distinct(CONTROL, .keep_all = TRUE) %>%
  summarize(MaintofZero = sum(WEIGHT)*2,
            CostClass = 'Maintenance') %>%
  rename(Year = AHSYEAR)

# Create dataset_list, containing Final and ZeroMaint
dataset_list <- list(Final, ZeroMaint)

# Output a long version of the AHS Summary along with ZeroMaint (ZeroMaint will output to sheet #2)
write.xlsx(dataset_list, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Long_HarvardWeighted.xlsx")

Final <- Final %>%
  pivot_wider(id_cols = CostClass, names_from = Year, values_from = c(Doers, Jobs, TotalJobCost))

# Output a wide version of the AHS Summary
write.xlsx(Final, "C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/AHSSummaries/AHSSummarywithMini_Wide_HarvardWeighted.xlsx")
