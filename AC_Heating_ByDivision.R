library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)
library(conflicted)

conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

Data <- Data %>%
  mutate(JOBDIY = ifelse(JOBDIY == 'NR', NA, JOBDIY),
         Count = 1)

RecentYears <- c(seq(2015, 2021, 2))
for (i in RecentYears) {
  
  assign(paste0('Design_', i),
         value = svydesign(data = get(paste0("Data_", i)), id = ~1, weights = get(paste0("Data_", i))$WEIGHT))
  
  assign(paste0("HVACDoers_", i), 
         value = 
           Data %>% 
           filter(AHSYEAR == i & TENURE == "Owned/Bought" & JOBTYPE %in% c(26, 27)) %>%
           mutate(JOBTYPE = case_when(JOBTYPE == 26 ~ 'AC',
                                      JOBTYPE == 27 ~ 'Heating')) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           group_by(JOBTYPE, DIVISION) %>%
           summarize(Doers = round(sum(WEIGHT, na.rm = TRUE)),
                     Year = i))
  
  assign(paste0('HVACCounts_', i),
         value = svyby(~Count, ~JOBTYPE +DIVISION, design = get(paste0("Design_", i)), svytotal, na.rm = TRUE))
  
  assign(paste0('HVACCounts_', i),
         value = as.data.frame(get(paste0("HVACCounts_", i))) %>%
           mutate(Year = i) %>%
           rename(Jobs = Count) %>%
           select(-se)) 
  
  assign(paste0('HVACCounts_', i),
         value = get(paste0("HVACCounts_", i)) %>%
           filter(JOBTYPE %in% c(26,27)) %>%
           mutate(JOBTYPE = case_when(JOBTYPE == 26 ~ 'AC',
                                      JOBTYPE == 27 ~ 'Heating')))
  
  assign(paste0('Final_', i),
         value = get(paste0("HVACCounts_", i)) %>%
                 left_join(get(paste0("HVACDoers_", i)), by = c('JOBTYPE', 'DIVISION', 'Year')) %>%
                 mutate(Households = get(paste0("Households_", i))$Households,
                        ProjectsPerDoer = round(Jobs/Doers, 2),
                        IncRate = round(Doers/Households, 3)))
  
  
}

Final <- HVACCounts_2021 %>%
  left_join(HVACDoers_2021, by = c('JOBTYPE', 'DIVISION', 'Year'))


Test <- Final %>%
  mutate(Households = Households_2021$Households,
         ProjectsPerDoer = round(Jobs/Doers, 2),
         IncRate = round(Doers/Households, 3))


Final <- Final_2015 %>%
  rbind(Final_2017, Final_2019, Final_2021)

Final %>%
  filter(JOBTYPE == 'AC') %>%
  ggplot(aes(Year, IncRate)) +
  geom_line() +
  facet_wrap(~DIVISION)
