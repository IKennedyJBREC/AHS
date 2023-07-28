library(tidyverse)
library(openxlsx)
library(xts)
library(zoo)
library(lubridate)
library(eply)
library(readxl)

# Read in all datasets output from Ian's original script, drop a few irrelevant columns (Lookup, Concrete_Foundations, Concrete_Floors, & Concrete_Exterior_Walls),
# and round HVAC (just rounding to check values against the BID output)

Old_NC_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 1) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls)) %>%
  mutate(HVAC = round(HVAC))

Old_NC_Normalized <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 2) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls))

Old_RR_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 3) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls)) %>%
  mutate(HVAC = round(HVAC))

Old_RR_Normalized <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 4) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls))

Old_Total_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 5) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls)) %>%
  mutate(HVAC = round(HVAC))

Old_Total_Normalized <- read.xlsx("C:/Users/ikennedy/Downloads/Normalized_NC_RR.xlsx", sheet = 6) %>%
  select(-c(Lookup, Concrete_Foundations, Concrete_Floors, Concrete_Exterior_Walls))


# Read in all Raw datasets from the BID output, convert the 1st row to column names, and drop the first row/column (both are irrelevant)
New_Total_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 2)
names(New_Total_Raw) <- New_Total_Raw[1,]
New_Total_Raw <- New_Total_Raw[-1,-1]

New_RR_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 3)
names(New_RR_Raw) <- New_RR_Raw[1,]
New_RR_Raw <- New_RR_Raw[-1,-1]


New_NC_Raw <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 4)
names(New_NC_Raw) <- New_NC_Raw[1,]
New_NC_Raw <- New_NC_Raw[-1,-1]

# Read in all Standardized datasets from the BID output, drop the first two rows, then convert the #rd (now the 1st because 1-2 were dropped) row to column names, 
# and drop the new first row/column (both are irrelevant)
New_Total_Standardized <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 5)
New_Total_Standardized <- New_Total_Standardized[-c(1:2),]
names(New_Total_Standardized) <- New_Total_Standardized[1,]
New_Total_Standardized <- New_Total_Standardized[-1,-1]

New_RR_Standardized <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 6)
New_RR_Standardized <- New_RR_Standardized[-c(1:2),]
names(New_RR_Standardized) <- New_RR_Standardized[1,]
New_RR_Standardized <- New_RR_Standardized[-1,-1]

New_NC_Standardized <- read.xlsx("C:/Users/ikennedy/Downloads/Building Products Demand Meter_Q2 23 update.xlsx", sheet = 7)
New_NC_Standardized <- New_NC_Standardized[-c(1:2),]
names(New_NC_Standardized) <- New_NC_Standardized[1,]
New_NC_Standardized <- New_NC_Standardized[-1,-1]


# All Raw checks follow the same pipeline:
# Convert all values to numeric, round the HVAC variable for comparison, and create a data.frame containing 'TRUE' where values do not match. 
# Then sum these values (TRUE = 1, FALSE = 0) to check if any values mismatch (the sum() call)
# If the sum() call results in 0, you are good to go!

New_RR_Raw <- New_RR_Raw %>%
  sapply(FUN = as.numeric) 
New_RR_Raw <- as.data.frame(New_RR_Raw) %>%
  mutate(HVAC = round(HVAC))
RR_Raw_Check <- New_RR_Raw != Old_RR_Raw
sum(RR_Raw_Check[,c(1:18, 20)])

New_NC_Raw <- New_NC_Raw %>%
  sapply(FUN = as.numeric) 
New_NC_Raw <- as.data.frame(New_NC_Raw) %>%
  mutate(HVAC = round(HVAC))
NC_Raw_Check <- New_NC_Raw != Old_NC_Raw
sum(NC_Raw_Check[,c(1:18, 20)])

New_Total_Raw <- New_Total_Raw %>%
  sapply(FUN = as.numeric) 
New_Total_Raw <- as.data.frame(New_Total_Raw) %>%
  mutate(HVAC = round(HVAC))
Total_Raw_Check <- New_Total_Raw != Old_Total_Raw
sum(Total_Raw_Check)


# All Sta dardized  checks follow the same pipeline:
# Convert all values to numeric and create a data.frame containing 'TRUE' where values do not match. 
# Then sum these values (TRUE = 1, FALSE = 0) to check if any values mismatch (the sum() call)
# If the sum() call results in 0, you are good to go!
New_Total_Standardized <- New_Total_Standardized %>%
  sapply(FUN = as.numeric) 
Total_Standardized_Check <- New_Total_Standardized != Old_Total_Normalized
sum(Total_Standardized_Check)

New_RR_Standardized <- New_RR_Standardized %>%
  sapply(FUN = as.numeric) 
RR_Standardized_Check <- New_RR_Standardized != Old_RR_Normalized
sum(RR_Standardized_Check)

New_NC_Standardized <- New_NC_Standardized %>%
  sapply(FUN = as.numeric) 
NC_Standardized_Check <- New_NC_Standardized != Old_NC_Normalized
sum(NC_Standardized_Check)
