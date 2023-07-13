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

# This line must be run independently of all other lines, and will ask for the user to enter the file path to 2015-2021 data in the console below!
Data_2015_2021_Filepath <- readline(prompt = "Enter File Path to 2015-2021 data...No quoation marks!: ")

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
Years <- as.character(c(2011, 2013))

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

# Rename the TAXINC variable to TAXCRD to match other years
Household_2013 <- Household_2013 %>%
  rename(TAXCRD = TAXINC)

# Remove unnecessary objects
rm(YearFolders, UnzippedFileLocation, i, FilesToPull)

# Read in the Variable Matching sheet for Household variables
HouseholdVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 1)

# Filter for household variables relevant to 2021 (All non-NA values within the 'variable' column)
HouseholdVariables <- HouseholdVariables %>%
  filter(Year == 2011 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2021
HouseholdVariables <- HouseholdVariables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
HouseholdVariables <- HouseholdVariables[HouseholdVariables != "DEGREE"]

# Read in the Variable Matching sheet for Project variables
ProjectVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 2)

# Filter for project variables relevant to 2021 (All non-NA values within the 'variable' column)
ProjectVariables <- ProjectVariables %>%
  filter(Year == 2011 & !is.na(Variable))

# Create a vector storing the relevant project variables for 2021
ProjectVariables <- ProjectVariables$Variable


# Create a list of all 2015-2021 houshold datasets
HouseholdList <- list(Household_2011, Household_2013)
# Create a list of all 2015-2021 project datasets
ProjectList <- list(Project_2011, Project_2013)


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
Data_2011 <- Household_2011 %>%
  left_join(Project_2011, by = 'CONTROL')
Data_2013 <- Household_2013 %>%
  left_join(Project_2013, by = 'CONTROL')

# Remove all 2011-2013 household/project datasets, all this data now lies in the 'Data_[YEAR] datasets
rm(Household_2011, Household_2013, Project_2011, Project_2013, ProjectList, HouseholdList, selected_datasets, 
   i, SelectColumns, VariablesNow, Years, ProjectVariables)



# Custom function for reclassifying variables
Reclassify <- function(Data) {
  
  Data <- Data %>%
    
    # Unquote all variables
    mutate_all(.funs = unquote) %>%
    
    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(RAS == '01' ~ 'DisRepairs',
                                   RAS %in% c('03', '09', '35', '02', '07', '73', '05', '36', '08', '06', '10') ~ 'RoomAdd',
                                   RAS == '71' ~ 'Bathroom',
                                   RAS =='72' ~ 'Kitchen',
                                   RAS %in% c('11', '14', '12', '13', '67') ~ 'OutsideAtt',
                                   RAS %in% c('37', '38', '45', '15') ~ 'Exterior',
                                   RAS %in% c('49', '51', '52', '53', '55', '64') ~ 'Interior',
                                   RAS %in% c('40', '47', '42', '74', '57', '58', '61', '62', '63') ~ 'Systems',
                                   RAS %in% c('60', '65', '66', '68', '69', '70') ~ 'LotYardOther')) %>%
    # Create a 'CSA' variable
    mutate(CSA = case_when(SMSA == "0520" ~ 'ATL',
                           SMSA == "0720" ~ 'BAL',
                           SMSA == "1000" ~ 'BIR',
                           SMSA == "1120" ~ 'BOS',
                           SMSA %in% c("1600", "9991") ~ 'CHI',
                           SMSA == "1640" ~ 'CIN',
                           SMSA == "1680" ~ 'CLE',
                           SMSA == "1920" ~ 'DAL',
                           SMSA == "2080" ~ 'DEN',
                           SMSA == "2160" ~ 'DET',
                           SMSA == "3360" ~ 'HOU',
                           SMSA == "3760" ~ 'KC',
                           SMSA == "4120" ~ 'LV',
                           SMSA == "4480" ~ 'LA',
                           SMSA == "4920" ~ 'MEM',
                           SMSA == "5000" ~ 'MIA',
                           SMSA == "5080" ~ 'MIL',
                           SMSA == "5120" ~ 'MIN',
                           SMSA == "5560" ~ 'NO',
                           SMSA %in% c("5600", "9992", "9993") ~ 'NYC',
                           SMSA == "5880" ~ 'OKC',
                           SMSA == "6160" ~ 'PHI',
                           SMSA == "6200" ~ 'PHX',
                           SMSA == "6280" ~ 'PIT',
                           #SMSA == "38900" ~ 'POR',
                           SMSA == "6640" ~ 'RAL',
                           #SMSA == "40060" ~ 'RIC',
                           SMSA == "6780" ~ 'RIV',
                           SMSA == "6840" ~ 'ROC',
                           SMSA == "7240" ~ 'SA',
                           SMSA == "7360" ~ 'SF',
                           SMSA == "7400" ~ 'SJ',
                           SMSA == "7600" ~ 'SEA',
                           SMSA == "8280" ~ 'TAM',
                           SMSA == "8840" ~ 'DC',
                           SMSA == "9999" ~ 'Not in Metro')) %>%
    
    # Edit the CSA variable to read 'Others' for those in 2011-2013 but not in 2015-2021
    mutate(CSA = ifelse(is.na(CSA), 'Others', CSA)) %>%
    
    # Amend the DIVISION variable
    mutate(DIVISION = case_when(DIVISION == "01" ~ 'New England',
                                DIVISION == "02" ~ 'Mid Atlantic',
                                DIVISION == "03" ~ 'East North Central',
                                DIVISION == "04" ~ 'West North Central',
                                DIVISION == "56" ~ 'South Atlantic/East South Central',
                                DIVISION == "07" ~ 'West South Central',
                                DIVISION == "89" ~ 'Mountain/Pacific')) %>%
    
    # Amend the ZINC2 variable (-6 here is NA, -9 here is 'Not Reported')
    # ZINC2 = HINCP (2021)
    mutate(ZINC2 = case_when(ZINC2 %in% c('99999', '-6') ~ NA, 
                             ZINC2 %in%  c('99998', '-9') ~ 'NR',
                             !ZINC2 %in% c('-6', '-9', '99999', '99998') ~ ZINC2)) %>%
    
    # Amend the CSTMNT variable (-6 here is NA, -9 here is 'Not Reported')
    # CSTMNT = MAINTAMT (2021)
    mutate(CSTMNT = case_when(CSTMNT == '-6' ~ NA,
                              CSTMNT %in% c('-9', '.') ~ 'NR',
                              !CSTMNT %in% c('-6', '-9', '.') ~ CSTMNT)) %>%
    
    # Amend the VALUE variable (-6 here is NA, -9 here is 'Not Reported')
    # VALUE = MARKETVAL (2021)
    mutate(VALUE = case_when(VALUE %in% c('99999', '-6') ~ NA,
                             VALUE %in%  c('99998', '-9') ~ 'NR',
                             !VALUE %in% c('-6', '-9', '99999', '99998') ~ VALUE)) %>%
    
    # Amend the HHAGE variable (-6 here is NA)
    mutate(HHAGE = ifelse(HHAGE == '-6', NA, HHAGE)) %>%
    
    # Amend the TENURE variable (-6 here is NA)
    mutate(TENURE = case_when(TENURE == '1' ~ 'Owned/Bought',
                              TENURE == '2' ~ 'Rented',
                              TENURE == '3' ~ 'Occupied Without Rent',
                              TENURE == '-6' ~ NA)) %>%
    
    # Amend the NUNIT2 variable
    # NUNIT2 = BLD (2021)
    mutate(NUNIT2 = case_when(NUNIT2 == '1' ~ '1-Family Detached',
                              NUNIT2 == '2' ~ '1-Family Attached',
                              NUNIT2 == '3' ~ '2+ Apartments',
                              NUNIT2 == '4' ~ 'Manufactured/Mobile')) %>%
    
    # Amend the ISTATUS variable
    # ISTATUS = INTSTATUS (2021)
    mutate(ISTATUS = case_when(ISTATUS == '1' ~ 'Occupied',
                               ISTATUS == '2' ~ 'URE',
                               ISTATUS == '3' ~ 'Vacant')) %>%
    
    # Create the VACANCY_RECLASS variable, grouping into 'Year Round Vacant', 'Seasonal', & 'Occupied' (-6 here is NA)
    mutate(VACANCY_RECLASS = case_when(VACANCY %in% c('01', '02', '03', '04', '05') ~ 'Year Round Vacant',
                                       VACANCY %in% c('06', '08', '09', '10', '11') ~ 'Seasonal',
                                       VACANCY == '-6' & ISTATUS == 'Occupied' ~ 'Occupied',
                                       VACANCY == '-6' & ISTATUS != 'Occupied' ~ NA)) %>%
    
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

    # Amend the RAH variable
    # RAH = JOBDIY (2021)
    mutate(RAH = case_when(RAH == '1' ~ 'DIY',
                           RAH == '2' ~ 'Not DIY')) %>%
  
    # Amend the HHMOVE variable (-6 here is NA)
    mutate(HHMOVE = ifelse(HHMOVE == '-6', NA, HHMOVE)) %>%
    
    # Amend the RAD variable (-9 here is 'Not Reported')
    # RAD = JOBCOST (2021)
    mutate(RAD = ifelse(RAD == '-9', 'NR', RAD)) %>%
    
    # Create the HMRENEFF variable from ENEFIC (-6 here is NA, -9 here is 'Not Reported')
    mutate(HMRENEFF = case_when(ENEFIC == '1' ~ 'Yes',
                                ENEFIC == '2' ~ 'No',
                                ENEFIC == '-6' ~ NA, 
                                ENEFIC == '-9' ~ 'NR',
                                ENEFIC == ' ' ~ NA)) %>%
    
    # Rename variables to match 2015-2021
    rename(BLD = NUNIT2, HINCP = ZINC2, INTSTATUS = ISTATUS, MAINTAMT = CSTMNT, MARKETVAL = VALUE, 
           OMB13CBSA = SMSA, WEIGHT = WGT90GEO, YRBUILT = BUILT, JOBTYPE = RAS, JOBDIY = RAH, JOBCOST = RAD)
}

# Finalize the 2011-2013 data by using the custom function above
Data_2011 <- Reclassify(Data_2011) %>%
  # Specify AHSYEAR is 2011
  mutate(AHSYEAR = 2011)

Data_2013 <- Reclassify(Data_2013) %>%
  mutate(AHSYEAR = 2013)

Data_2011_2013 <- Data_2011 %>%
  # Attach 2017-2021 datasets to 2015 by row
  rbind(Data_2013) %>%
  # Convert numeric columns to numeric values
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         HHAGE = as.numeric(HHAGE),
         HHMOVE = as.numeric(HHMOVE),
         #REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST)) 

#Read in column names from 2015-2021 cleaned data
ColOrderCheck <- names(read.xlsx(Data_2015_2021_Filepath))
#Specify the column order for 2011-2013
ColOrder <- ColOrderCheck[ColOrderCheck %in% names(Data_2011_2013)]
#Find columns missing from 2011-2013 though present in 2015-2021
MissingCols <- ColOrderCheck[!ColOrderCheck %in% ColOrder]

# Reorder 2011-2013 data to match the column order in 2015-2021
Data_2011_2013 <- Data_2011_2013[ColOrder]

# Read in the 2015-2021 data and store as Data_2015_2021
Data_2015_2021 <- read.xlsx(Data_2015_2021_Filepath)

# Bind the 2011-2013 data to the 2015-2021 data. rbind.fill() fills all columns from 2011-2013 that are present in 2015-2021 (binding them as new rows)
# 2011-2013 data will have NA values for all of the 'MissingCols'
Data_2011_2021 <- Data_2015_2021 %>%
  rbind.fill(Data_2011_2013) 

# Order the 2011-2021 data by year
Data_2011_2021 <- Data_2011_2021[order(Data_2011_2021$AHSYEAR), ]

# Create a 'LogicCheck' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998' 
# These are all typical NA classifications for AHS variables, if LogicCheck is empty after running the following line, you are good to go! 
# If it is not empty, check the rows for potentially missed NA values.
LogicCheck <- Data_2011_2021 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998')))

# Output the dataset to the specified file path
write.xlsx(Data_2011_2021, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_2011_2021.xlsx")
