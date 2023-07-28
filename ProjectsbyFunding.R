library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)

# Read in the raw AHS data
Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

Data_2021 <- Data %>%
  # Filter the data for 2021 only, where TENURE is 'Owned/Bought', 'JobCategory' is not 'DisRepairs' and JOBCOST > 0
  filter(AHSYEAR == 2021 & TENURE == "Owned/Bought" & JobCategory != 'DisRepairs' & JOBCOST > 0) %>%
  # Create a 'CostClass' variable for ranges of JOBCOSTs
  mutate(CostClass = case_when(JOBCOST < 10000 ~ '$0 - $10,000',
                               JOBCOST >= 10000 & JOBCOST < 20000 ~ '$10,000 - $20,000',
                               JOBCOST >= 20000 & JOBCOST < 30000 ~ '$20,000 - $30,000',
                               JOBCOST >= 30000 & JOBCOST < 40000 ~ '$30,000 - $40,000',
                               JOBCOST >= 40000 & JOBCOST < 50000 ~ '$40,000 - $50,000',
                               JOBCOST >= 50000 & JOBCOST < 60000 ~ '$50,000 - $60,000',
                               JOBCOST >= 60000 & JOBCOST < 70000 ~ '$60,000 - $70,000',
                               JOBCOST >= 70000 & JOBCOST < 100000 ~ '$70,000 - $100,000',
                               JOBCOST >= 100000 ~ '$100,000+'))

# Data_Summary will contain Raw/Weighted Project Counts for each CostClass/JOBFUNDS pair
Data_Summary <- Data_2021 %>%
  # Group by CostClass and Funding Source
  group_by(CostClass, JOBFUNDS) %>%
  # Find the Raw Project Count (Counts) and Weighted Project Count (Weight)
  summarize(Counts = n(),
            Weight = sum(WEIGHT, na.rm = TRUE)) 

# Total_Summary will contain Raw/Weighted Project Counts for each CostClass (i.e. total Project Counts for each 'CostClass')
Total_Summary <- Data_2021 %>%
  # Group by CostClass
  group_by(CostClass) %>%
  # Find the Raw Project Count (Counts) and Weighted Project Count (Weight)
  summarize(TotalCounts = n(),
            TotalWeightedCount = sum(WEIGHT)) 

Data_Summary <- Data_Summary %>%
  # Join Total_Summary to Data_Summary by the 'CostClass'
  left_join(Total_Summary, by = 'CostClass') %>%
        # Create 'RawPercentCapture' by dividing Raw Counts ('Counts') by 'TotalCounts'
  mutate(RawPercentCapture = Counts/TotalCounts * 100,
         # Create 'WeightedPercentCapture' by dividing Weighted Counts ('Weight') by the 'TotalWeightedCount'
         WeightedPercentCapture = Weight/TotalWeightedCount*100)


Data_Summary <- Data_Summary %>%
  # Change any '-1' values to 'Not Reported'
  mutate(JOBFUNDS = ifelse(JOBFUNDS == -1, 'Not Reported', JOBFUNDS)) %>%
  # Rename a few columns
  rename(WeightedProjectCount = Weight, TotalRawCount = TotalCounts, RawProjectCount = Counts)

# Output the summary data
write.xlsx(Data_Summary, "C:/Users/ikennedy/Downloads/ProjectsByFundingSource.xlsx")


# Exploratory Plots

Data_Summary %>%
  filter(JOBFUNDS %in% c("Cash, Refinance",  "Home Equity Line") & !is.na(CostClass) & CostClass != '$100,000+') %>%
  ggplot(aes(CostClass, WeightedPercentCapture, fill = JOBFUNDS)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  geom_text(aes(label = round(WeightedPercentCapture, 1), group = JOBFUNDS),
            position = position_stack(vjust = .4), # Adjust the vertical position of labels
            color = "black", size = 6) +
  geom_text(aes(label = paste0('SS: ', round(WeightedProjectCount))),
            position = position_stack(vjust = .8)) +
  labs(title = 'Share of Remodeling Projects Financed, by Total Project Spend', y = 'Percent') +
  theme(plot.background = element_rect(color = '#12213E', fill = '#12213E'),
        panel.background = element_rect(color = '#12213E', fill = '#12213E'),
        axis.title = element_text(size = 28, color = '#1E6FD9', family = 'Freight Black'),
        title = element_text(size = 30, color = '#1E6FD9', family = 'Freight Black'),
        panel.grid.major = element_line(color = 'white', size = .1),
        axis.text = element_text(color = '#1E6FD9', size = 20, family = 'Freight Black'),
        strip.text = element_text(size = 28, color = '#00CE89', family = 'Freight Black'),
        strip.background = element_rect(fill = '#12213E'),
        legend.background = element_rect(fill = '#12213E'),
        legend.text = element_text(size = 16, color = '#00CE89', family = 'Freight Black'))


Data_Summary %>%
  filter(!is.na(CostClass) & CostClass != '$100,000+') %>%
  ggplot(aes(CostClass, WeightedPercentCapture, fill = JOBFUNDS)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  geom_text(aes(label = round(WeightedPercentCapture, 1), group = JOBFUNDS),
            position = position_stack(vjust = .4), # Adjust the vertical position of labels
            color = "black", size = 6) +
  # geom_text(aes(label = paste0('SS: ', round(WeightedProjectCount))),
  #           position = position_stack(vjust = .8)) +
  labs(title = 'Share of Remodeling Projects Financed, by Total Project Spend', y = 'Precent') +
  theme(plot.background = element_rect(color = '#12213E', fill = '#12213E'),
        panel.background = element_rect(color = '#12213E', fill = '#12213E'),
        axis.title = element_text(size = 28, color = '#1E6FD9', family = 'Freight Black'),
        title = element_text(size = 30, color = '#1E6FD9', family = 'Freight Black'),
        panel.grid.major = element_line(color = 'white', size = .1),
        axis.text = element_text(color = '#1E6FD9', size = 20, family = 'Freight Black'),
        strip.text = element_text(size = 28, color = '#00CE89', family = 'Freight Black'),
        strip.background = element_rect(fill = '#12213E'),
        legend.background = element_rect(fill = '#12213E'),
        legend.text = element_text(size = 16, color = '#00CE89', family = 'Freight Black'))
