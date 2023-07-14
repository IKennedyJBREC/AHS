library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_2001_2021.xlsx")

Years <- 2021

for (i in Years) {
  assign(paste0("Data_", i), 
         value = Data %>%
           mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
           filter(!is.na(JOBCOST) & AHSYEAR == i) %>%
           mutate(JobCategory = as.factor(JobCategory)))
}


Data_2021 <- Data_2021 %>%
  mutate(JobSize = case_when(JOBCOST < 1800 ~ 'Mini',
                             JOBCOST >= 1800 & JOBCOST < 10000 ~ 'Small',
                             JOBCOST >= 10000 & JOBCOST < 30000 ~ 'Medium',
                             JOBCOST >= 30000 ~ 'Large')) 

SS <- Data_2021 %>%
  group_by(JobCategory, JobSize) %>%
  summarize(SampleSize = n())

SS2 <- Data_2021 %>%
  group_by(JobSize) %>%
  summarize(JobCategory = 'All',
            SampleSize = n())
SS2 <- SS2[,c(2,1,3)]

Design <- svydesign(data = Data_2021, id = ~1, weights = Data_2021$WEIGHT)


SurveyTotal <-svyby(~JOBCOST, ~JobCategory + JobSize, design = Design, FUN = svytotal, na.rm = TRUE)
SurveyTotal2 <-svyby(~JOBCOST, ~JobSize, design = Design, FUN = svytotal, na.rm = TRUE)
SurveyTotal2$JobCategory <- 'All'
SurveyTotal2 <- SurveyTotal2[,c(4,1:3)]

SurveyTotal <- SurveyTotal %>%
  rbind(SurveyTotal2)

SurveyTotal <- SurveyTotal %>%
  left_join(SS, by = c('JobCategory', 'JobSize'))

SurveyTotal <- SurveyTotal %>%
  left_join(SS2, by = c('JobCategory', 'JobSize')) %>%
  mutate(SampleSize.x = ifelse(is.na(SampleSize.x), SampleSize.y, SampleSize.x)) %>%
  rename(SampleSize = SampleSize.x) %>%
  select(-SampleSize.y)

font_add("FreightBlack", regular = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf", 
         italic = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf")

SurveyTotal %>%
  ggplot(aes(x = factor(JobSize, levels = c("Mini", "Small", "Medium", "Large")), y = JOBCOST/1000000000)) +
  geom_col(fill = '#00CE89') +
  theme_fivethirtyeight() +
  geom_text(aes(label = SampleSize), size = 6, nudge_y = 4, color = '#00CE89') +
  labs(title = 'Total Job Cost by Cost Class & Size, 2021', y = 'Total Job Cost (Billions of $)', x = 'Cost Class', 
       subtitle = 'Mini: < $1,800, Small: $1,800 - $10,000, Medium: $10,000 - $30,000, Large: >= $30,000') +
  theme(plot.background = element_rect(color = '#12213E', fill = '#12213E'),
        panel.background = element_rect(color = '#12213E', fill = '#12213E'),
        axis.title = element_text(size = 28, color = '#1E6FD9', family = 'Freight Black'),
        title = element_text(size = 30, color = '#1E6FD9', family = 'Freight Black'),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = '#1E6FD9', size = 20, family = 'Freight Black'),
        strip.text = element_text(size = 28, color = '#00CE89', family = 'Freight Black'),
        strip.background = element_rect(fill = '#12213E')) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~JobCategory, scales ='free_y')

ggsave("C:/Users/ikennedy/Downloads/GraphTest1.png", width = 36, height = 12, dpi = 300)
Test <- add_logo("C:/Users/ikennedy/Downloads/GraphTest1.png", logo_path = "C:/Users/ikennedy/Downloads/logo-dark.png", logo_position = 'top right')
Test
