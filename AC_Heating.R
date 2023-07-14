library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)
library(conflicted)
library(blscrapeR)

conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), dplyr::select(), base::as.numeric())

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")
Inflation <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AHSProject/InflationRate.xlsx")
Inflation <- Inflation[,c(1,3)]


Data <- Data %>%
  mutate(JOBDIY = ifelse(JOBDIY == 'NR', NA, JOBDIY),
         Count = 1)


Years <- c(seq(1995, 2021, 2))
for (i in Years) {
  assign(paste0('Data_', i),
         value = Data %>%
           filter(AHSYEAR == i & !is.na(JobCategory) & TENURE == "Owned/Bought") %>%
           mutate(JOBCOSTInflated = JOBCOST * Inflation[Inflation$Year == i,]$Rate))

  assign(paste0("Households_", i), 
         value = Data %>% 
           filter(AHSYEAR == i & TENURE == "Owned/Bought") %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           summarize(Households = sum(WEIGHT, na.rm = TRUE)))
}

#RecentYears <- c(seq(2015, 2021, 2))
RecentYears <- c(seq(2015, 2021, 2))
for (i in RecentYears) {
  
  assign(paste0('Data_', i),
         value = Data %>%
           mutate(JOBCOSTInflated = JOBCOST * Inflation[Inflation$Year == i,]$Rate) %>%
           filter(AHSYEAR == i & !is.na(JobCategory) & TENURE == "Owned/Bought" & JOBCOSTInflated >= 1500))
  
  assign(paste0('Design_', i),
         value = svydesign(data = get(paste0("Data_", i)), id = ~1, weights = get(paste0("Data_", i))$WEIGHT))
  
  assign(paste0("HVACDoers_", i), 
         value = 
           Data %>% 
           mutate(JOBCOSTInflated = JOBCOST * Inflation[Inflation$Year == i,]$Rate) %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought" & JOBTYPE %in% c(26, 27) & JOBCOSTInflated >= 1500) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           group_by(JOBTYPE) %>%
           summarize(Doers = round(sum(WEIGHT, na.rm = TRUE)),
                     Year = i))
  
  assign(paste0('HVACCounts_', i),
         value = svyby(~Count, ~JOBTYPE, design = get(paste0("Design_", i)), svytotal, na.rm = TRUE))

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
         value = data.frame(JOBTYPE = get(paste0("HVACCounts_", i))$JOBTYPE,
                            Jobs = get(paste0("HVACCounts_", i))$Jobs,
                            Doers = get(paste0("HVACDoers_", i))$Doers,
                            Households = get(paste0("Households_", i))$Households,
                            Year = get(paste0("HVACCounts_", i))$Year) %>%
           mutate(ProjectsPerDoer = Jobs/Doers,
                  IncRate = Doers/Households))
  
  
}




OldYears <- c(seq(1995, 2013, 2))

for (i in OldYears) {
  
  assign(paste0('Data_', i),
         value = Data %>%
           mutate(JOBCOSTInflated = JOBCOST * Inflation[Inflation$Year == i,]$Rate) %>%
           filter(AHSYEAR == i & !is.na(JobCategory) & TENURE == "Owned/Bought" & JOBCOSTInflated >= 1500))
  
  assign(paste0('Design_', i),
         value = svydesign(data = get(paste0("Data_", i)), id = ~1, weights = get(paste0("Data_", i))$WEIGHT))
  
  assign(paste0("HVACDoers_", i), 
         value = 
           Data %>% 
           mutate(JOBCOSTInflated = JOBCOST * Inflation[Inflation$Year == i,]$Rate) %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought" & JOBTYPE %in% c(57, 58, 59) & JOBCOSTInflated >= 1500) %>%
           mutate(JOBTYPE = case_when(JOBTYPE == 57 ~ 'AC',
                                      JOBTYPE %in% c(57, 58) ~ 'Heating')) %>%
           distinct(CONTROL, .keep_all = TRUE) %>%
           group_by(JOBTYPE) %>%
           summarize(Doers = round(sum(WEIGHT, na.rm = TRUE)),
                     Year = i))
  
  assign(paste0('HVACCounts_', i),
         value = svyby(~Count, ~JOBTYPE, design = get(paste0("Design_", i)), svytotal, na.rm = TRUE))
  
  assign(paste0('HVACCounts_', i),
         value = as.data.frame(get(paste0("HVACCounts_", i))) %>%
           mutate(Year = i) %>%
           rename(Jobs = Count) %>%
           select(-se)) 
  
  assign(paste0('HVACCounts_', i),
         value = get(paste0("HVACCounts_", i)) %>%
           filter(JOBTYPE %in% c(57, 58, 59)) %>%
           mutate(JOBTYPE = case_when(JOBTYPE == 57 ~ 'AC',
                                      JOBTYPE %in% c(57, 58) ~ 'Heating')))
  
  assign(paste0('Final_', i),
         value = data.frame(JOBTYPE = get(paste0("HVACCounts_", i))$JOBTYPE,
                            Jobs = get(paste0("HVACCounts_", i))$Jobs,
                            Doers = get(paste0("HVACDoers_", i))$Doers,
                            Households = get(paste0("Households_", i))$Households,
                            Year = get(paste0("HVACCounts_", i))$Year) %>%
           mutate(ProjectsPerDoer = Jobs/Doers,
                  IncRate = Doers/Households))
  
  
}

Final <- Final_1999 %>%
  rbind(Final_2001, Final_2003, Final_2005, Final_2007, Final_2009, Final_2011, Final_2013, Final_2015,
        Final_2017, Final_2019, Final_2021) %>%
  mutate(ProjectsPerDoer = round(ProjectsPerDoer, 2),
         IncRate = round(IncRate, 3),
         Jobs = round(Jobs),
         Households = round(Households)) %>%
  arrange(JOBTYPE, Year)

Final %>%
  ggplot(aes(Year, IncRate)) +
  geom_line() +
  scale_x_continuous(limits = c(1998,2022), breaks = c(seq(1999,2021, 2))) +
  theme_fivethirtyeight() +
  labs(title = 'Incidence Rate by Year') +
  facet_wrap(~JOBTYPE, scales = 'free_y')

Final %>%
  ggplot(aes(Year, ProjectsPerDoer)) +
  geom_line() +
  scale_x_continuous(limits = c(1998,2022), breaks = c(seq(1999,2021, 2))) +
  theme_fivethirtyeight() +
  labs(title = 'Projects/Doer by Year') +
  facet_wrap(~JOBTYPE, scales = 'free_y')


write.xlsx(Final, "C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/AC_Heating_Yearly_InflationCorrect.xlsx")
