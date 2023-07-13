library(tidyverse)
library(utils)
library(openxlsx)
library(eply)
library(plyr)
library(conflicted)
# Prefer dplyr over all other packages
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate())

# Set the working directory to the location of the unzipped file folders (i.e. one folder for each year)
# This line must be run independently of all other lines, and will ask for the user to enter the file path to Unzipped File Folders in the console below!
setwd(readline(prompt = "Enter File Path to Unzipped File Folders...No quoation marks!: "))

# Transfer the file path entered above to 'UnzippedFileLocation'
UnzippedFileLocation <- getwd()

UnzippedFileLocation
# Before running any further code, make sure the file folders are located in the directory printed in the console below
# If the folders are located somewhere else, re-run line 12 to specify the correct location! 
# Each year should have its own folder (i.e. '1997', '1999', etc.)

# Define the 'FilesToPull'. If needing to include additional files, paste them at the end of 'FilesToPull' as follows:
## Example for all files: FilesToPull <- c('household.csv', 'project.csv', 'mortgage.csv', 'people.csv)
## Make sure to uncomment portions of the for-in loop below to read in mortgage/person data!
FilesToPull <- c('household.csv', 'project.csv')

# Define the years of survey coverage for files that will be read in. 1995 is omitted here given it does not have a project/household file
Years <- as.character(c(seq(2015, 2021, 2)))

# Create a vector of folder locations for each survey year, this vector should contain file paths to the Yearly Unzipped File Folders (one for each year)
YearFolders <- paste0(getwd(), '/', Years)

# For each folder year
for (i in 1:length(YearFolders)) {
  
  # Assign the variable 'Household_{YEAR}' for each houshold csv
  assign(paste0("Household_", Years[i]), 
         value = read.csv(paste0(YearFolders[i], '/' , FilesToPull[1])))
  
  # Assign the variable 'Project_{YEAR}' for each household csv
  assign(paste0("Project_", Years[i]), 
                value = read.csv(paste0(YearFolders[i], '/' , FilesToPull[2])))
  
  # # Assign the variable 'Mortgage_{YEAR}' for each mortgage csv
  # assign(paste0("Mortgage_", Years[i]), 
  #        value = read.csv(paste0(YearFolders[i], '/' , FilesToPull[3])))
  # 
  # # Assign the variable 'Person_{YEAR}' for each person csv
  # assign(paste0("Person_", Years[i]), 
  #        value = read.csv(paste0(YearFolders[i], '/' , FilesToPull[4])))
  
}
# Remove unnecessary objects
rm(YearFolders, UnzippedFileLocation, i, FilesToPull)


# Read in the Variable Matching sheet for Household variables
HouseholdVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 1)

# Filter for household variables relevant to 2021 (All non-NA values within the 'variable' column)
HouseholdVariables <- HouseholdVariables %>%
  filter(Year == 2021 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2021
HouseholdVariables <- HouseholdVariables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
HouseholdVariables <- HouseholdVariables[HouseholdVariables != "DEGREE"]

# Read in the Variable Matching sheet for Project variables
ProjectVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 2)

# Filter for project variables relevant to 2021 (All non-NA values within the 'variable' column)
ProjectVariables <- ProjectVariables %>%
  filter(Year == 2021 & !is.na(Variable))

# Create a vector storing the relevant project variables for 2021
ProjectVariables <- ProjectVariables$Variable


# Create a list of all 2015-2021 houshold datasets
HouseholdList <- list(Household_2015, Household_2017, Household_2019, Household_2021)
# Create a list of all 2015-2021 project datasets
ProjectList <- list(Project_2015, Project_2017, Project_2019, Project_2021)


# Function to select columns and retain names
SelectColumns <- function(data, columns) {
  selected_data <- data[, columns, drop = FALSE]
  return(selected_data)
}

# Apply the function to each dataset
selected_datasets <- lapply(ProjectList, SelectColumns, columns = ProjectVariables)

# For each folder year
for (i in 1:length(Years)) {
  # Assign the variable 'Project_{YEAR}' for each amended 'project' dataset
  assign(paste0("Project_", Years[i]), 
         value = as.data.frame(selected_datasets[i]))
}

# Remove 'selected_datasets', this variable will be used again below
rm(selected_datasets)

# Select the appropriate columns from the housing data
selected_datasets <- lapply(HouseholdList, SelectColumns, columns = HouseholdVariables)

# For each folder year
for (i in 1:length(Years)) {
  # Assign the variable 'Household_{YEAR}' for each amended 'household' dataset
  assign(paste0("Household_", Years[i]), 
         value = as.data.frame(selected_datasets[i]))
}

# Finalize the 2015-2021 data by joining amended household/project datasets
Data_2015 <- Household_2015 %>%
  left_join(Project_2015, by = 'CONTROL')
Data_2017 <- Household_2017 %>%
  left_join(Project_2017, by = 'CONTROL')
Data_2019 <- Household_2019 %>%
  left_join(Project_2019, by = 'CONTROL')
Data_2021 <- Household_2021 %>%
  left_join(Project_2021, by = 'CONTROL')

# Remove all 2015-2021 household/project datasets, all this data now lies in the 'Data_[YEAR] datasets
rm(Household_2015, Household_2017, Household_2019, Household_2021, Project_2015, Project_2017, Project_2019, Project_2021, HouseholdList, ProjectList, selected_datasets, i, SelectColumns)
rm(VariablesNow_NeedtoLocate, VariablesLater, VariablesNow, Years, ProjectVariables)

# Custom function for reclassifying variables
Reclassify <- function(Data) {
  
  Data <- Data %>%
    
    # Unquote all variables
   mutate_all(.funs = unquote) %>%
    
    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(JOBTYPE %in% c('01', '02', '03', '04', '05', '06') ~ 'DisRepairs',
                                   JOBTYPE %in% c('07', '08', '09', '10', '11') ~ 'RoomAdd',
                                   JOBTYPE %in% c('12') ~ 'Bathroom',
                                   JOBTYPE %in% c('13') ~ 'Kitchen',
                                   JOBTYPE %in% c('14', '15') ~ 'OutsideAtt',
                                   JOBTYPE %in% c('16', '17', '18', '19') ~ 'Exterior',
                                   JOBTYPE %in% c('20', '25', '31') ~ 'Interior',
                                   JOBTYPE %in% c('21', '22', '23', '24', '26', '27', '29', '30') ~ 'Systems',
                                   JOBTYPE %in% c('28', '32', '33', '34', '35', '36', '37') ~ 'LotYardOther')) %>%
    # Create a 'CSA' variable
    mutate(CSA = case_when(OMB13CBSA == "12060" ~ 'ATL',
                           OMB13CBSA == "12580" ~ 'BAL',
                           OMB13CBSA == "13820" ~ 'BIR',
                           OMB13CBSA == "14460" ~ 'BOS',
                           OMB13CBSA == "16980" ~ 'CHI',
                           OMB13CBSA == "17140" ~ 'CIN',
                           OMB13CBSA == "17460" ~ 'CLE',
                           OMB13CBSA == "19100" ~ 'DAL',
                           OMB13CBSA == "19740" ~ 'DEN',
                           OMB13CBSA == "19820" ~ 'DET',
                           OMB13CBSA == "26420" ~ 'HOU',
                           OMB13CBSA == "28140" ~ 'KC',
                           OMB13CBSA == "29820" ~ 'LV',
                           OMB13CBSA == "31080" ~ 'LA',
                           OMB13CBSA == "32820" ~ 'MEM',
                           OMB13CBSA == "33100" ~ 'MIA',
                           OMB13CBSA == "33340" ~ 'MIL',
                           OMB13CBSA == "33460" ~ 'MIN',
                           OMB13CBSA == "35380" ~ 'NO',
                           OMB13CBSA == "35620" ~ 'NYC',
                           OMB13CBSA == "36420" ~ 'OKC',
                           OMB13CBSA == "37980" ~ 'PHI',
                           OMB13CBSA == "38060" ~ 'PHX',
                           OMB13CBSA == "38300" ~ 'PIT',
                           OMB13CBSA == "38900" ~ 'POR',
                           OMB13CBSA == "39580" ~ 'RAL',
                           OMB13CBSA == "40060" ~ 'RIC',
                           OMB13CBSA == "40140" ~ 'RIV',
                           OMB13CBSA == "40380" ~ 'ROC',
                           OMB13CBSA == "41700" ~ 'SA',
                           OMB13CBSA == "41860" ~ 'SF',
                           OMB13CBSA == "41940" ~ 'SJ',
                           OMB13CBSA == "42660" ~ 'SEA',
                           OMB13CBSA == "45300" ~ 'TAM',
                           OMB13CBSA == "47900" ~ 'DC',
                           OMB13CBSA == "99998" ~ 'Others',
                           OMB13CBSA == "99999" ~ 'Not in Metro')) %>%
    
    # Amend the DIVISION variable
    mutate(DIVISION = case_when(DIVISION == "1" ~ 'New England',
                                DIVISION == "2" ~ 'Mid Atlantic',
                                DIVISION == "3" ~ 'East North Central',
                                DIVISION == "4" ~ 'West North Central',
                                DIVISION == "5" ~ 'South Atlantic',
                                DIVISION == "6" ~ 'East South Central',
                                DIVISION == "7" ~ 'West South Central',
                                DIVISION == "8" ~ 'Mountain',
                                DIVISION == "9" ~ 'Pacific')) %>%
    
    # Amend the HINCP variable (-6 here is NA)
    mutate(HINCP = ifelse(HINCP == '-6', NA, HINCP)) %>%
    
    # Amend the MAINTAMT variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(MAINTAMT = case_when(MAINTAMT == '-6' ~ NA,
                                MAINTAMT == '-9' ~ 'NR',
                                !MAINTAMT %in% c('-6', '-9') ~ MAINTAMT)) %>%
    
    # Amend the MARKETVAL variable (-6 here is NA)
    mutate(MARKETVAL = ifelse(MARKETVAL == '-6', NA, MARKETVAL)) %>%
    
    # Amend the HHAGE variable (-6 here is NA)
    mutate(HHAGE = ifelse(HHAGE == '-6', NA, HHAGE)) %>%
    
    # Amend the TENURE variable (-6 here is NA)
    mutate(TENURE = case_when(TENURE == '1' ~ 'Owned/Bought',
                              TENURE == '2' ~ 'Rented',
                              TENURE == '3' ~ 'Occupied Without Rent',
                              TENURE == '-6' ~ NA)) %>%
    
    # Amend the REMODAMT variable (-6 here is NA)
    mutate(REMODAMT = ifelse(REMODAMT == '-6', NA, REMODAMT)) %>%
    
    # Amend the TOTBALAMT variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(TOTBALAMT = case_when(TOTBALAMT == '-6' ~ NA,
                                 TOTBALAMT == '-9' ~ 'NR',
                                 !TOTBALAMT %in% c('-6', '-9') ~ TOTBALAMT)) %>%
    
    # Amend the BLD variable
    mutate(BLD = case_when(BLD == '01' ~ 'Mobile',
                           BLD == '02' ~ '1-Family Detached',
                           BLD == '03' ~ '1-Family Attached',
                           BLD == '04' ~ '2 Apartments',
                           BLD == '05' ~ '3-4 Apartments',
                           BLD == '06' ~ '5-9 Apartments',
                           BLD == '07' ~ '10-19 Apartments',
                           BLD == '08' ~ '20-49 Apartments',
                           BLD == '09' ~ '50+ Apartments',
                           BLD == '10' ~ 'Boat, RV, Van, Etc.')) %>%
    
    # Amend the INTSTATUS variable
    mutate(INTSTATUS = case_when(INTSTATUS == '1' ~ 'Occupied',
                                 INTSTATUS == '2' ~ 'URE',
                                 INTSTATUS == '3' ~ 'Vacant')) %>%
    
    # Create the VACANCY_RECLASS variable, grouping into 'Year Round Vacant', 'Seasonal', & 'Occupied' (-6 here is NA)
    mutate(VACANCY_RECLASS = case_when(VACANCY %in% c('01', '02', '03', '04', '05') ~ 'Year Round Vacant',
                                       VACANCY %in% c('06', '08', '09', '10', '11') ~ 'Seasonal',
                                       VACANCY == '-6' & INTSTATUS == 'Occupied' ~ 'Occupied',
                                       VACANCY == '-6' & INTSTATUS != 'Occupied' ~ NA)) %>%
    
    # Amend the VACANCY variable (-6 here is NA)
    mutate(VACANCY = case_when(VACANCY %in% c('01', '04') ~ 'For Rent or Rented',
                               VACANCY == '02' ~ 'Rent or Sale',
                               VACANCY %in% c('03', '05') ~ 'For Sale or Sold',
                               #VACANCY == '04' ~ 'Rent, Not Yet Occupied',
                               #VACANCY == '05' ~ 'Sold, Not Yet Occupied',
                               VACANCY == '06' ~ 'Occasional Use',
                               VACANCY == '07' ~ 'Other',
                               VACANCY == '08' ~ 'Seasonal, Summer Only',
                               VACANCY == '09' ~ 'Seasonal, Winter Only',
                               VACANCY == '10' ~ 'Seasonal, Other',
                               VACANCY == '11' ~ 'Migratory',
                               VACANCY == '-6' ~ NA)) %>%
    
    # Amend the VACANCY variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(GUTREHB = case_when(GUTREHB == '1' ~ 'Yes',
                               GUTREHB == '2' ~ 'No',
                               GUTREHB == '-9' ~ 'NR',
                               GUTREHB == '-6' ~ NA)) %>%
    
    # Amend the HMRACCESS variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRACCESS = case_when(HMRACCESS == '1' ~ 'Yes',
                                 HMRACCESS == '2' ~ 'No',
                                 HMRACCESS == '-9' ~ 'NR',
                                 HMRACCESS == '-6' ~ NA)) %>%
    
    # Amend the HMRENEFF variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRENEFF = case_when(HMRENEFF == '1' ~ 'Yes',
                                HMRENEFF == '2' ~ 'No',
                                HMRENEFF == '-9' ~ 'NR',
                                HMRENEFF == '-6' ~ NA)) %>%
    
    # Amend the HMRSALE variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRSALE = case_when(HMRSALE == '1' ~ 'Yes',
                               HMRSALE == '2' ~ 'No',
                               HMRSALE == '-9' ~ 'NR',
                               HMRSALE == '-6' ~ NA)) %>%
    
    # Amend the JOBDIY variable
    mutate(JOBDIY = case_when(JOBDIY == '1' ~ 'DIY',
                              JOBDIY == '2' ~ 'Not DIY')) %>%
    
    # Amend the JOBCOST variable (-9 here is 'Not Reported')
    mutate(JOBCOST = ifelse(JOBCOST == '-9', 'NR', JOBCOST)) %>%
    
    # Amend the JOBCOMP variable (-9 here is 'Not Reported')
    mutate(JOBCOMP = case_when(JOBCOMP == '1' ~ 'Completed',
                               JOBCOMP == '2' ~ 'Not Completed',
                               JOBCOMP == '-9' ~ 'NR')) %>%
    
    # Amend the JOBCOMPYR variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(JOBCOMPYR = case_when(JOBCOMPYR == '1' ~ '20XX - 2',
                               JOBCOMPYR == '2' ~ '20XX - 1',
                               JOBCOMPYR == '3' ~ '20XX',
                               JOBCOMPYR == '-9' ~ 'NR',
                               JOBCOMPYR == '-6' ~ NA)) %>%
    
    # Amend the JOBWORKYR variable (-6 here is NA, -9 here is 'Not Reported')
    mutate(JOBWORKYR = case_when(JOBWORKYR == '1' ~ '20XX - 2',
                                 JOBWORKYR == '2' ~ '20XX - 1',
                                 JOBWORKYR == '3' ~ '20XX',
                                 JOBWORKYR == '-9' ~ 'NR',
                                 JOBWORKYR == '-6' ~ NA)) %>%
    
    # Amend the JOBFUNDS variable (-9 here is 'Not Reported')
    mutate(JOBFUNDS = case_when(JOBFUNDS == '1' ~  'Cash, Savings',
                                JOBFUNDS == '2' ~  'Cash, Refinance',
                                JOBFUNDS == '3' ~  'Home Equity Line',
                                JOBFUNDS == '4' ~  'Homeowner Insurance Settlement',
                                JOBFUNDS == '5' ~  'Credit/Retail Card',
                                JOBFUNDS == '6' ~  'Contractor Financing',
                                JOBFUNDS == '7' ~  'Other',
                                JOBFUNDS == '-9' ~  'NR')) %>%
    
    # Amend the HHMOVE variable (-6 here is NA)                           
    mutate(HHMOVE = ifelse(HHMOVE == '-6', NA, HHMOVE)) %>%
    
    # Drop the original CSA variable
    select(-c(OMB13CBSA))
}

# Finalize the 2015-2021 data by using the custom function above
Data_2015 <- Reclassify(Data_2015) %>%
  mutate(AHSYEAR = 2015) %>%
  # Amend the JOBCOMPYR variable to read the correct year for 2015 data
  mutate(JOBCOMPYR = case_when(JOBCOMPYR == '20XX - 2' ~ (AHSYEAR - 2),
                               JOBCOMPYR == '20XX - 1' ~ (AHSYEAR - 1),
                               JOBCOMPYR == '20XX' ~ AHSYEAR)) %>%
  # Amend the JOBWORKYR variable to read the correct year for 2015 data
  mutate(JOBWORKYR = case_when(JOBWORKYR == '20XX - 2' ~ (AHSYEAR - 2),
                               JOBWORKYR == '20XX - 1' ~ (AHSYEAR - 1),
                               JOBWORKYR == '20XX' ~ AHSYEAR))

Data_2017 <- Reclassify(Data_2017) %>%
  mutate(AHSYEAR = 2017) %>%
  # Amend the JOBCOMPYR variable to read the correct year for 2017 data
  mutate(JOBCOMPYR = case_when(JOBCOMPYR == '20XX - 2' ~ (AHSYEAR - 2),
                               JOBCOMPYR == '20XX - 1' ~ (AHSYEAR - 1),
                               JOBCOMPYR == '20XX' ~ AHSYEAR))  %>%
  # Amend the JOBWORKYR variable to read the correct year for 2017 data
  mutate(JOBWORKYR = case_when(JOBWORKYR == '20XX - 2' ~ (AHSYEAR - 2),
                                JOBWORKYR == '20XX - 1' ~ (AHSYEAR - 1),
                                JOBWORKYR == '20XX' ~ AHSYEAR))

Data_2019 <- Reclassify(Data_2019 )%>%
  mutate(AHSYEAR = 2019) %>%
  # Amend the JOBCOMPYR variable to read the correct year for 2019 data
  mutate(JOBCOMPYR = case_when(JOBCOMPYR == '20XX - 2' ~ (AHSYEAR - 2),
                               JOBCOMPYR == '20XX - 1' ~ (AHSYEAR - 1),
                               JOBCOMPYR == '20XX' ~ AHSYEAR))  %>%
  # Amend the JOBWORKYR variable to read the correct year for 2019 data
  mutate(JOBWORKYR = case_when(JOBWORKYR == '20XX - 2' ~ (AHSYEAR - 2),
                                JOBWORKYR == '20XX - 1' ~ (AHSYEAR - 1),
                                JOBWORKYR == '20XX' ~ AHSYEAR))

Data_2021 <- Reclassify(Data_2021) %>%
  mutate(AHSYEAR = 2021) %>%
  # Amend the JOBCOMPYR variable to read the correct year for 2021 data
  mutate(JOBCOMPYR = case_when(JOBCOMPYR == '20XX - 2' ~ (AHSYEAR - 2),
                               JOBCOMPYR == '20XX - 1' ~ (AHSYEAR - 1),
                               JOBCOMPYR == '20XX' ~ AHSYEAR))  %>%
  # Amend the JOBWORKYR variable to read the correct year for 2021 data
  mutate(JOBWORKYR = case_when(JOBWORKYR == '20XX - 2' ~ (AHSYEAR - 2),
                                JOBWORKYR == '20XX - 1' ~ (AHSYEAR - 1),
                                JOBWORKYR == '20XX' ~ AHSYEAR))


Data_2015_2021 <- Data_2015 %>%
  # Attach 2017-2021 datasets to 2015 by row
  rbind(Data_2017, Data_2019, Data_2021) %>%
  # Convert columns that should be numeric to numeric values (they are character strings up to this point)
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         HHAGE = as.numeric(HHAGE),
         HHMOVE = as.numeric(HHMOVE),
         REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST)) 


# Create a 'LogicCheck' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998' 
# These are all typical NA classifications for AHS variables, if LogicCheck is empty after running the following line, you are good to go! 
# If it is not empty, check the rows for potentially missed NA values.

LogicCheck <- Data_2015_2021 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998')))

# Output the dataset to the specified file path
write.xlsx(Data_2015_2021, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_2015_2021.xlsx")
