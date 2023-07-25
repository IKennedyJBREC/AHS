library(tidyverse)
library(openxlsx)
library(priceR)
library(sysfonts)
library(xts)
library(zoo)
library(lubridate)
library(eply)
library(readxl)

# Function to normalize values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# These lines must be run independently of all other lines, and will ask for the user to enter the file path to the Raw NC/RR data, as well as an output location for the final data
NC_RR_Raw_Filepath <- readline(prompt = "Enter File Path to the Raw NC/RR data...No quoation marks!: ")
OutputFilepath <- readline(prompt = "Enter File Path to the desired output location...No quoation marks!: ")

# Read in the Raw NC/RR data
DataNC <- read.xlsx(NC_RR_Raw_Filepath, sheet = 3)
DataRR <- read.xlsx(NC_RR_Raw_Filepath, sheet = 4)

# Combine heating/cooling data into one category: HVAC
DataNC <- DataNC %>%
  mutate(HVAC = Heating_Systems+Cooling_Systems, Type = 'NC') %>%
  select(-c(Heating_Systems, Cooling_Systems))

# Combine heating/cooling data into one category: HVAC
DataRR <- DataRR %>%
  mutate(HVAC = Heating_Systems+Cooling_Systems, Type = 'RR') %>%
  select(-c(Heating_Systems, Cooling_Systems))

# Bind the NC/RR together by row and save as Total
Total <- DataNC %>%
  plyr::rbind.fill(DataRR)

# Find the total job spend by category
Total <- Total %>%
  # Group by the lookup (i.e. year/quarter), this assures that only values within the same year/quarter will be summed
  group_by(Lookup) %>%
  summarize(Concrete_Foundations = sum(Concrete_Foundations, na.rm = TRUE),
            Concrete_Floors = sum(Concrete_Floors, na.rm = TRUE),
            Concrete_Exterior_Walls = sum(Concrete_Exterior_Walls, na.rm = TRUE),
            Dimensional_Lumber = sum(Dimensional_Lumber, na.rm = TRUE),
            Sheathing = sum(Sheathing, na.rm = TRUE),
            Insulation = sum(Insulation, na.rm = TRUE),
            Siding = sum(Siding, na.rm = TRUE),
            Roofing = sum(Roofing, na.rm = TRUE),
            Windows = sum(Windows, na.rm = TRUE),
            Doors = sum(Doors, na.rm = TRUE),
            Flooring = sum(Flooring, na.rm = TRUE),
            Cabinets = sum(Cabinets, na.rm = TRUE),
            Countertops = sum(Countertops, na.rm = TRUE),
            Faucets = sum(Faucets, na.rm = TRUE),
            Plumbing_Fixtures = sum(Plumbing_Fixtures, na.rm = TRUE),
            Drywall_Wallboard = sum(Drywall_Wallboard, na.rm = TRUE),
            Interior_Paint = sum(Interior_Paint, na.rm = TRUE),
            Appliances = sum(Appliances, na.rm = TRUE),
            Decks_Porches = sum(Decks_Porches, na.rm = TRUE),
            Concrete = sum(Concrete, na.rm = TRUE),
            HVAC = sum(HVAC, na.rm = TRUE))

# Drop 'Type' from DataNC
DataNC <- DataNC %>%
  select(-Type)
# Select the time based columns and save as 'Time'
Time <- DataNC[,c(1:3)]
# Drop the time-based columns from the NC data
DataNC <- DataNC[,-c(1:3)]
# Normalize the NC data
Normalized <- DataNC %>%
  # Apply the function 'normalize' (see lines 12-13) to each column of the dataframe and save as Normalized
  lapply(FUN = normalize)
# Convert Normalized back to a dataframe (lapply converts it to a nested list)
Normalized <- as.data.frame(Normalized)
# Bind the Time columns back onto the Normalized NC data
DataNC_Normalized <- Time %>%
  cbind(Normalized)

# Drop 'Type' from DataRR
DataRR <- DataRR %>%
  select(-Type)
# Select the time based columns and save as 'Time'
Time <- DataRR[,c(1:3)]
# Drop the time-based columns from the RR data
DataRR <- DataRR[,-c(1:3)]
# Normalize the RR data
Normalized <- DataRR %>%
  # Apply the function 'normalize' (see lines 12-13) to each column of the dataframe and save as Normalized (nested list)
  lapply(FUN = normalize)
# Convert Normalized back to a dataframe (lapply converts it to a nested list)
Normalized <- as.data.frame(Normalized)
# Bind the Time columns back onto the Normalized RR data
DataRR_Normalized <- Time %>%
  cbind(Normalized)


# Select the Lookup column and save as 'Time'
Time <- Total$Lookup
# Drop the Lookup column from 'Total'
Total <- Total[,-1]
# Normalize the 'Total' data
Normalized <- Total %>%
  # Apply the function 'normalize' (see lines 12-13) to each column of the dataframe and save as Normalized (nested list)
  lapply(FUN = normalize)
# Convert Normalized back to a dataframe (lapply converts it to a nested list)
Normalized <- as.data.frame(Normalized)
# Bind the Time column back onto the Normalized 'Total' data
Total_Normalized <- Time %>%
  cbind(Normalized)

# Remove intermediate files
rm(DataNC, DataRR, Total, Normalized, Time)

# Reestablish the time-based columns from the RR Normalized data
Times <- DataRR_Normalized[,c(1:3)]

# Drop the 'Lookup' column in 'Total_Normalized'
Total_Normalized <- Total_Normalized[,-1]
# Bind the correct time columns to 'Total_Normalized'
Total_Normalized <- Total_Normalized %>%
  cbind(Times)
# Reorder columns in 'Total_Normalized' to match the other normalized dataframes
Total_Normalized <- Total_Normalized[,c(22:24, 1:21)]

# Read back in the raw NC/RR data
# The next portion will recreate the 'Total' dataset without normalizing
DataNC <- read.xlsx(NC_RR_Raw_Filepath, sheet = 3)
DataRR <- read.xlsx(NC_RR_Raw_Filepath, sheet = 4)

# Combine heating/cooling data into one category: HVAC
DataNC <- DataNC %>%
  mutate(HVAC = Heating_Systems+Cooling_Systems) %>%
  select(-c(Heating_Systems, Cooling_Systems))

# Combine heating/cooling data into one category: HVAC
DataRR <- DataRR %>%
  mutate(HVAC = Heating_Systems+Cooling_Systems) %>%
  select(-c(Heating_Systems, Cooling_Systems))

# Bind the NC/RR together by row and save as 'Total'
Total <- DataNC %>%
  plyr::rbind.fill(DataRR)

# Find the total job spend by category
Total <- Total %>%
  group_by(Lookup) %>%
  summarize(Concrete_Foundations = sum(Concrete_Foundations, na.rm = TRUE),
            Concrete_Floors = sum(Concrete_Floors, na.rm = TRUE),
            Concrete_Exterior_Walls = sum(Concrete_Exterior_Walls, na.rm = TRUE),
            Dimensional_Lumber = sum(Dimensional_Lumber, na.rm = TRUE),
            Sheathing = sum(Sheathing, na.rm = TRUE),
            Insulation = sum(Insulation, na.rm = TRUE),
            Siding = sum(Siding, na.rm = TRUE),
            Roofing = sum(Roofing, na.rm = TRUE),
            Windows = sum(Windows, na.rm = TRUE),
            Doors = sum(Doors, na.rm = TRUE),
            Flooring = sum(Flooring, na.rm = TRUE),
            Cabinets = sum(Cabinets, na.rm = TRUE),
            Countertops = sum(Countertops, na.rm = TRUE),
            Faucets = sum(Faucets, na.rm = TRUE),
            Plumbing_Fixtures = sum(Plumbing_Fixtures, na.rm = TRUE),
            Drywall_Wallboard = sum(Drywall_Wallboard, na.rm = TRUE),
            Interior_Paint = sum(Interior_Paint, na.rm = TRUE),
            Appliances = sum(Appliances, na.rm = TRUE),
            Decks_Porches = sum(Decks_Porches, na.rm = TRUE),
            Concrete = sum(Concrete, na.rm = TRUE),
            HVAC = sum(HVAC, na.rm = TRUE))

# Drop the Lookup column from 'Total'
Total <- Total[-1]
# Reestablish the time-based columns from the RR Normalized data
Total <- Total %>%
  cbind(Times)
# Reorder columns in 'Total' to match the other dataframes
Total <- Total[,c(22:24, 1:21)]

# Remove unnessecary files
rm(Times, normalize)

# Multiply all normalized values by 100 for a 0-100 scale (columns 1-3 are time-based columns and are ignored here)
DataNC_Normalized[,c(4:24)] <- DataNC_Normalized[,c(4:24)]*100
DataRR_Normalized[,c(4:24)] <- DataRR_Normalized[,c(4:24)]*100
Total_Normalized[,c(4:24)] <- Total_Normalized[,c(4:24)]*100

# Specify a list of names for raw/normalized data, the names listed in quotes will correspond to the names of each worksheet in the output excel
dataset_names <- list("NC-Raw" = DataNC, "NC-Normalized" = DataNC_Normalized, 
                      "RR-Raw" = DataRR, "RR-Normalized" = DataRR_Normalized, 
                      "Total-Raw" = Total, "Total-Normalized" = Total_Normalized)

# Output the raw/normalized data to the file path specified
write.xlsx(dataset_names, file = OutputFilepath)

