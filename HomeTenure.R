library(tidyverse)
library(ggthemes)

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

Years <- c(seq(1995, 2021, 2))

for (i in Years){
  assign(paste0('HomeTenure_', i), 
         value = Data %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought") %>%
           mutate(TimeInHome = i-HHMOVE) %>%
    select(AHSYEAR, DIVISION, TimeInHome))
}

HomeTenure <- HomeTenure_1995 %>%
  rbind(HomeTenure_1997, HomeTenure_1999, HomeTenure_2001, HomeTenure_2003, HomeTenure_2005, HomeTenure_2007, HomeTenure_2009,
        HomeTenure_2011, HomeTenure_2013, HomeTenure_2015, HomeTenure_2017, HomeTenure_2019, HomeTenure_2021)

HomeTenure <- HomeTenure %>%
  group_by(AHSYEAR, DIVISION) %>%
  summarize(MedianTenure = median(TimeInHome, na.rm = TRUE),
            MeanTenure = mean(TimeInHome, na.rm = TRUE))

HomeTenure %>%
  ggplot(aes(AHSYEAR, MedianTenure)) +
  geom_col() +
  scale_y_continuous(limits = c(0,13), breaks = c(seq(0,13,1))) +
  scale_x_continuous(limits = c(2000, 2022), breaks = c(seq(2001,2021,2))) +
  labs(title = 'Median Tenure by Year', x = 'Year', y = 'Tenure') +
  theme_bw() 


# Division Plots
Years <- c(seq(2015, 2021, 2))

for (i in Years){
  assign(paste0('HomeTenure_', i), 
         value = Data %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought") %>%
           mutate(TimeInHome = i-HHMOVE) %>%
           select(AHSYEAR, DIVISION, TimeInHome))
}
HomeTenure <- HomeTenure_2015 %>%
  rbind(HomeTenure_2017, HomeTenure_2019, HomeTenure_2021)

HomeTenure <- HomeTenure %>%
  group_by(AHSYEAR, DIVISION) %>%
  summarize(MedianTenure = median(TimeInHome, na.rm = TRUE),
            MeanTenure = mean(TimeInHome, na.rm = TRUE))

HomeTenure %>%
  ggplot(aes(AHSYEAR, MedianTenure)) +
  geom_col() +
  scale_y_continuous(limits = c(0,15), breaks = c(seq(0,15,1))) +
  scale_x_continuous(limits = c(2014, 2023), breaks = c(seq(2015,2021,2))) +
  labs(title = 'Median Tenure by Year', x = 'Year', y = 'Tenure') +
  theme_bw() +
  facet_wrap(~DIVISION)



# Division Plots
Years <- c(seq(2011, 2021, 2))

for (i in Years){
  assign(paste0('HomeTenure_', i), 
         value = Data %>%
           filter(AHSYEAR == i & TENURE == "Owned/Bought") %>%
           mutate(TimeInHome = i-HHMOVE) %>%
           select(AHSYEAR, DIVISION, TimeInHome))
}
HomeTenure <- HomeTenure_2011 %>%
  rbind(HomeTenure_2013, HomeTenure_2015, HomeTenure_2017, HomeTenure_2019, HomeTenure_2021)

HomeTenure <- HomeTenure %>%
  mutate(DIVISION = case_when(DIVISION %in% c("South Atlantic", "East South Central") ~  "South Atlantic/East South Central",
                              DIVISION %in% c("Mountain", "Pacific") ~  "Mountain/Pacific",
                              !DIVISION %in% c("South Atlantic", "East South Central", "Mountain", "Pacific") ~  DIVISION)) %>%
  group_by(AHSYEAR, DIVISION) %>%
  summarize(MedianTenure = median(TimeInHome, na.rm = TRUE),
            MeanTenure = mean(TimeInHome, na.rm = TRUE))

HomeTenure %>%
  ggplot(aes(AHSYEAR, MedianTenure)) +
  geom_col() +
  scale_y_continuous(limits = c(0,15), breaks = c(seq(0,15,1))) +
  scale_x_continuous(limits = c(2010, 2023), breaks = c(seq(2011,2021,2))) +
  labs(title = 'Median Tenure by Year', x = 'Year', y = 'Tenure') +
  theme_bw() +
  facet_wrap(~DIVISION)
