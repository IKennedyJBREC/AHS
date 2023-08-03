library(tidyverse)
library(utils)
library(openxlsx)
library(eply)
library(plyr)
library(conflicted)
# Prefer dplyr over all other packages
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), base::as.numeric())

# This line must be run independently of all other lines, and will ask for the user to enter the file path to 2001-2021 data in the console below!
Data_1997_2021_Filepath <- readline(prompt = "Enter File Path to 1997-2021 data...No quoation marks!: ")

# Read in the 1995 data, 1995 is in a wide format
Data_1995 <- read.csv("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSProject/1995/ahs1995n.csv")

# Calculate the total number of Owner-Occupied Households
Households <- Data_1995 %>%
  distinct(CONTROL, .keep_all = TRUE) %>%
  filter(TENURE %in% c("'1'")) %>%
  summarize(Households = sum(WEIGHT))
Households <- round(Households$Households)



# Read in the Variable Matching sheet for Household variables
HouseholdVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 1)

# Filter for household variables relevant to 2021 (All non-NA values within the 'variable' column)
HouseholdVariables <- HouseholdVariables %>%
  filter(Year == 1995 & !is.na(Variable))
# Create a vector storing the relevant household variables for 2021
HouseholdVariables <- HouseholdVariables$Variable
# Remove 'DEGREE' as it is not accessible as of July, 2023
HouseholdVariables <- HouseholdVariables[!HouseholdVariables %in% c("DEGREE", "MOVE(1 - 15)", "RAC")]


# Read in the Variable Matching sheet for Project variables
ProjectVariables <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/AHS/AHSVariablesbyYear.xlsx", sheet = 2)

# Filter for project variables relevant to 2021 (All non-NA values within the 'variable' column)
ProjectVariables <- ProjectVariables %>%
  filter(Year == 1995 & !is.na(Variable)) 

# Create a vector storing the relevant project variables for 2021
ProjectVariables <- ProjectVariables$Variable
# Remove 'RAY(1 - 16)' from ProjectVariables as it is not accessible as of July, 2023
ProjectVariables <- ProjectVariables[!ProjectVariables %in% c("RAY(1 - 16)")]

# 1995 data is in a wide format, and features 16 unique columns (for each of the RAS, RAH, & RAD variables) for individual jobs. 
## i.e. each household contains columns 'RAS1', 'RAS2', 'RAS3', etc...'RAH1', 'RAH2', 'RAH3', etc...'RAD1', 'RAD2', 'RAD3', etc.
# Search for any columns that start with 'RAS', store them in 'selected_columns'
selected_columns <- grep("^RAS", names(Data_1995), value = TRUE)
# Search for any columns that start with 'RAH', store them in 'selected_columns2'
selected_columns2 <- grep("^RAH", names(Data_1995), value = TRUE)
# Search for any columns that start with 'RAD', store them in 'selected_columns3'
selected_columns3 <- grep("^RAD", names(Data_1995), value = TRUE)

# Create Variables_1995, storing all relevant 1995 variables (those within the Variable Matching Sheet + all RAS/RAH/RAD columns)
Variables_1995 <- c(HouseholdVariables, selected_columns, selected_columns2, selected_columns3)

# Remove the selected_columns datasets, these are all in Variable_1995 now
rm(selected_columns2, selected_columns3)

# Create 'Data_1995_Selected', selecting the variables of interest for 1995 Data
Data_1995_Selected <- Data_1995[,Variables_1995]
# Replace any "' '" entries (i.e. blank quotes) to NAs
Data_1995_Selected <- replace(Data_1995_Selected[,c(1:61)], Data_1995_Selected[,c(1:61)] == "' '", NA)

# Pivot the Selected Dataset to a long format for the RAS (JOBTYPE) variable
## Each Houshold has 16 'RAS' field, thus 16 rows for each Household will be created. Only RAS entries that are not NA will be relevant!
LongTest <- Data_1995_Selected %>%
  pivot_longer(cols = c(14:29), names_to = 'RASCount', values_to = 'RAS')

# Filter for RAS entries that are not NA...Non NA entries represent jobs
LongTest <- LongTest %>%
  filter(!is.na(RAS))

# Pivot the dataset to a long format again, now using the 'RAH' (JOBDIY) variable. Only RAH entries that are not NA will be relevant!
LongTest <- LongTest %>%
  pivot_longer(cols = c(14:29), names_to = 'RAHCount', values_to = 'RAH')

# Filter for RAH entries that are not NA...Non NA entries represent jobs
LongTest <- LongTest %>%
  filter(!is.na(RAH))

# Amend the 'RASCount' variable to a numeric value
LongTest <- LongTest %>%
  mutate(RASCount = case_when(RASCount == 'RAS1' ~ 1,
                              RASCount == 'RAS2' ~ 2,
                              RASCount == 'RAS3' ~ 3,
                              RASCount == 'RAS4' ~ 4,
                              RASCount == 'RAS5' ~ 5,
                              RASCount == 'RAS6' ~ 6,
                              RASCount == 'RAS7' ~ 7,
                              RASCount == 'RAS8' ~ 8,
                              RASCount == 'RAS9' ~ 9,
                              RASCount == 'RAS10' ~ 10,
                              RASCount == 'RAS11' ~ 11,
                              RASCount == 'RAS12' ~ 12,
                              RASCount == 'RAS13' ~ 13,
                              RASCount == 'RAS14' ~ 14,
                              RASCount == 'RAS15' ~ 15,
                              RASCount == 'RAS16' ~ 16,
                              is.na(RASCount) ~ NA))

# Amend the 'RAHCount' variable to a numeric value
LongTest <- LongTest %>%         
         mutate(RAHCount = case_when(RAHCount == 'RAH1' ~ 1,
                              RAHCount == 'RAH2' ~ 2,
                              RAHCount == 'RAH3' ~ 3,
                              RAHCount == 'RAH4' ~ 4,
                              RAHCount == 'RAH5' ~ 5,
                              RAHCount == 'RAH6' ~ 6,
                              RAHCount == 'RAH7' ~ 7,
                              RAHCount == 'RAH8' ~ 8,
                              RAHCount == 'RAH9' ~ 9,
                              RAHCount == 'RAH10' ~ 10,
                              RAHCount == 'RAH11' ~ 11,
                              RAHCount == 'RAH12' ~ 12,
                              RAHCount == 'RAH13' ~ 13,
                              RAHCount == 'RAH14' ~ 14,
                              RAHCount == 'RAH15' ~ 15,
                              RAHCount == 'RAH16' ~ 16,
                              is.na(RAHCount) ~ NA))

# Filter the long dataset for only rows in which RASCount matches RAHCount. This assures that jobs receive the correct 'DIY' variables (if present)
LongTest <- LongTest %>%
  filter(RAHCount == RASCount)

# Pivot the dataset to a long format again, now using the 'RAD' (JOBCOST) variable
LongTest <- LongTest %>%
  pivot_longer(cols = c(14:29), names_to = 'RADCount', values_to = 'RAD')

# Amend the 'RADCount' variable to a numeric value
LongTest <- LongTest %>%         
  mutate(RADCount = case_when(RADCount == 'RAD1' ~ 1,
                              RADCount == 'RAD2' ~ 2,
                              RADCount == 'RAD3' ~ 3,
                              RADCount == 'RAD4' ~ 4,
                              RADCount == 'RAD5' ~ 5,
                              RADCount == 'RAD6' ~ 6,
                              RADCount == 'RAD7' ~ 7,
                              RADCount == 'RAD8' ~ 8,
                              RADCount == 'RAD9' ~ 9,
                              RADCount == 'RAD10' ~ 10,
                              RADCount == 'RAD11' ~ 11,
                              RADCount == 'RAD12' ~ 12,
                              RADCount == 'RAD13' ~ 13,
                              RADCount == 'RAD14' ~ 14,
                              RADCount == 'RAD15' ~ 15,
                              RADCount == 'RAD16' ~ 16,
                              is.na(RADCount) ~ NA))

# Filter the long dataset for only rows in which RASCount matches RADCount. This assures that jobs receive the correct 'JOBCOST' variable
Data_1995 <- LongTest %>%
  # Assure that we are linking jobs to the correct cost
  filter(RASCount == RADCount) %>%
  # Drop the 'Count' variables...Those were only needed for within-household job-matching
  select(-c(RASCount, RADCount, RAHCount))
# Find the number of 'Doers'
Doers <- as.numeric(length(unique(Data_1995$CONTROL)))

# Custom function for creating Job Cateogory classes
Reclassify <- function(Data) {
  
  Data <- Data %>%
    
    # Unquote all variables
    mutate_all(.funs = unquote) %>%
    
    # Create a 'JobCategory' column for each job type
    mutate(JobCategory = case_when(RAS == '01' ~ 'DisRepairs',
                                   RAS %in% c('02', '03', '04', '05', '06', '07', '08', '09', '10', '35', '36', '73') ~ 'RoomAdd',
                                   RAS %in% c('16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '71') ~ 'Bathroom',
                                   RAS %in% c('26', '27', '28', '29', '30', '31', '32', '33', '34', '72') ~ 'Kitchen',
                                   RAS %in% c('11', '12', '13', '14', '67') ~ 'OutsideAtt',
                                   RAS %in% c('15', '37', '38', '39', '45', '46') ~ 'Exterior',
                                   RAS %in% c('49', '50', '51', '52', '53', '54', '55', '56', '64') ~ 'Interior',
                                   RAS %in% c('40', '41', '42', '43', '44', '47', '48', '57', '58', '59', '61', '62', '63', '74') ~ 'Systems',
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
                           SMSA %in% c("5600", "5640","9992", "9993") ~ 'NYC',
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
    
    # Amend the ZINC2 variable (-6 here is NA)
    # ZINC2 = HINCP (2021)
    mutate(ZINC2 = ifelse(ZINC2 == '-6', NA, ZINC2)) %>%
    
    # Amend the CSTMNT variable (-6 here is NA, -9 & . here are 'Not Reported')
    # CSTMNT = MAINTAMT (2021)
    mutate(CSTMNT = case_when(CSTMNT == '-6' ~ NA,
                              CSTMNT %in% c('-9', '.') ~ 'NR',
                              !CSTMNT %in% c('-6', '-9', '.') ~ CSTMNT)) %>%
    
    # Amend the VALUE variable (-6 here is NA, -9 here is 'Not Reported')
    # VALUE = MARKETVAL (2021)
    mutate(VALUE = case_when(VALUE == '-6' ~ NA,
                             VALUE == '-9' ~ 'NR',
                             !VALUE %in% c('-6', '-9') ~ VALUE)) %>%
    
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
                              NUNIT2 %in% c('4', '5') ~ 'Manufactured/Mobile')) %>%
    
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
                               VACANCY == '06' ~ 'Occasional Use',
                               VACANCY == '07' ~ 'Other',
                               VACANCY == '08' ~ 'Seasonal, Summer Only',
                               VACANCY == '09' ~ 'Seasonal, Winter Only',
                               VACANCY == '10' ~ 'Seasonal, Other',
                               VACANCY == '11' ~ 'Migratory',
                               VACANCY == '-6' ~ NA)) %>%
    
    # Amend the RAH variable (-6 here is NA, -9 here is 'Not Reported')
    # RAH = JOBDIY (2021)
    mutate(RAH = case_when(RAH == '1' ~ 'DIY',
                           RAH == '2' ~ 'Not DIY',
                           RAH == '-9' ~ 'NR',
                           RAH == '-6' ~ NA)) %>%
    
    # Amend the RAD variable (99999 & -6 here are NA, 99998 & -9 here are 'Not Reported')
    # RAD = JOBCOST (2021)
    mutate(RAD = case_when(RAD %in% c('99999', '-6') ~ NA,
                         RAD %in%  c('99998', '-9') ~ 'NR',
                         !RAD %in% c('-6', '-9', '99999', '99998') ~ RAD))  %>%
    
    # Rename variables to match 2015-2021
    rename(BLD = NUNIT2, HINCP = ZINC2, INTSTATUS = ISTATUS, MAINTAMT = CSTMNT, MARKETVAL = VALUE, OMB13CBSA = SMSA, YRBUILT = BUILT, 
                  JOBTYPE = RAS, JOBDIY = RAH, JOBCOST = RAD)
}

# Finalize the 1995 data by using the custom function above
Data_1995 <- Reclassify(Data_1995) %>%
  mutate(AHSYEAR = 1995)


# Function to replace 'NR' with -1 in a vector
ReplaceNR <- function(x) {
  ifelse(x == 'NR', -1, x)
}

# Function to apply the replacement to all columns in a data frame
ReplaceNR_Final <- function(Data) {
  Data %>% mutate(across(everything(), ReplaceNR))
}

# Apply ReplaceNR_Final() to the 1995 data, this will replace any 'NR' values with -1
Data_1995 <- ReplaceNR_Final(Data_1995)



Data_1995 <- Data_1995 %>%
  # Convert numeric columns to numeric values
  mutate(HINCP = as.numeric(HINCP),
         MARKETVAL = as.numeric(MARKETVAL),
         WEIGHT = as.numeric(WEIGHT),
         YRBUILT = as.numeric(YRBUILT),
         #HHAGE = as.numeric(HHAGE),
         #HHMOVE = as.numeric(HHMOVE),
         #REMODAMT = as.numeric(REMODAMT),
         JOBCOST = as.numeric(JOBCOST))

#Read in column names from 1997-2021 cleaned data
ColOrderCheck <- names(read.xlsx(Data_1997_2021_Filepath))
#Specify the column order for 1995
ColOrder <- ColOrderCheck[ColOrderCheck %in% names(Data_1995)]
#Find columns missing from 1995 though present in 2015-2021
MissingCols <- ColOrderCheck[!ColOrderCheck %in% ColOrder]

# Reorder 1995 data to match the column order in 2015-2021
Data_1995 <- Data_1995[ColOrder]

# Read in the 1997-2021 data and store as Data_1997_2021
Data_1997_2021 <- read.xlsx(Data_1997_2021_Filepath)

# Bind the 1995 data to the 1997-2021 data. rbind.fill() fills all columns from 1995 that are present in 1997-2021 (binding them as new rows)
# 1995 data will have NA values for all of the 'MissingCols'
Data_1995_2021 <- Data_1997_2021 %>%
  rbind.fill(Data_1995) 

# Order the 1995-2021 data by year
Data_1995_2021 <- Data_1995_2021[order(Data_1995_2021$AHSYEAR), ]

# Amend the BLD variable to match the classes in 2001-2021...1995-1999 BLD data uses 'Manufactured/Mobile' rather than 'Mobile' (as used in 2001-2021)
Data_1995_2021 <- Data_1995_2021 %>%
  mutate(BLD = case_when(BLD == 'Manufactured/Mobile' ~ 'Mobile',
                         BLD != 'Manufactured/Mobile' ~ BLD))

# Create a 'LogicCheck' dataframe with rows that contain a value (in ANY column!) equal to one of the following:
# -9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998' 
# These are all typical NA classifications for AHS variables, if LogicCheck is empty after running the following line, you are good to go! 
# If it is not empty, check the rows for potentially missed NA values.
LogicCheck <- Data_1997_2021 %>%
  filter_all(any_vars(. %in% c(-9, -6, '-6', '-9', '.', 'B', 99998, 99999, '99999', '99998')))

# Output the dataset to the specified file path
write.xlsx(Data_1995_2021, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")