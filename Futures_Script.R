library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)

NewFutures <- read.csv("C:/Users/ikennedy/Downloads/NewFutures.csv")

NewFutures <- NewFutures %>%
  mutate(Date = parse_date(Date, format = "%m/%d/%Y"),
         Volume = as.numeric(Volume)) %>%
  rename(Close = Close.Last) %>%
  select(Date, Close)


OldFutures <- read.csv("C:/Users/ikennedy/Downloads/OldFutures.csv")

OldFutures <- OldFutures %>%
  mutate(Date = parse_date(Date, format = "%m/%d/%Y")) %>%
  filter(!is.na(Close))

OldTest <- OldFutures %>%
  filter(Date > '2022-08-05' & Date < '2023-02-05')
NewTest <- NewFutures %>%
  filter(Date > '2022-08-05' & Date < '2023-02-05')

OldTest <- OldTest %>%
  filter(Date %in% NewTest$Date)

Old_New <- OldTest %>%
  left_join(NewTest, by = 'Date')

Old_New <- Old_New %>%
  mutate(Diff = Close.y - Close.x)
Diff <- mean(Old_New$Diff)


OldFutures <- OldFutures %>%
  filter(Date < '2022-08-05') %>%
  mutate(Diff = adjust_for_inflation(Diff, '2022-08-05', 'US', Date),
         Close_Amended = Close + Diff)
Differences <- OldFutures %>%
  select(Date, Diff)
OldFutures <- OldFutures %>%
  select(Date, Close_Amended) %>%
  rename(Close = Close_Amended)

OldFutures <- OldFutures %>%
  filter(Date < '2022-08-05')

Futures <- OldFutures %>%
  plyr::rbind.fill(NewFutures) %>%
  arrange(Date)

Futures$Diff <- NA

Futures <- Futures %>%
  left_join(Differences, by = 'Date') %>%
  rename(Old_New_Difference = Diff.y) %>%
  select(-Diff.x) %>%
  mutate(OldClose = Close - Old_New_Difference,
         OldClose = round(OldClose, 1),
         Close = round(Close, 1),
         Old_New_Difference = round(Old_New_Difference, 1)) 

write.xlsx(Futures, 'C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/Lumber Futures/AmendedFutures.xlsx')


OldFutures <- read.csv("C:/Users/ikennedy/Downloads/OldFutures.csv")

OldFutures <- OldFutures %>%
  mutate(Date = parse_date(Date, format = "%m/%d/%Y")) %>%
  filter(!is.na(Close) & Date > '2020-01-01')

Futures %>%
  ggplot(aes(Date, Close)) +
  geom_line(color = 'red', show.legend = TRUE) +
  geom_line(aes(Date, OldClose), color = 'blue') +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,1800,100))) +
  theme(axis.title = element_text(size = 28, family = 'Freight Black'),
        title = element_text(size = 30),
        axis.text = element_text(size = 24)) +
  labs(title = 'Amended Lumber Futures, 2022-Current', y = 'Close', subtitle = 'Blue = Old Futures, Red = New Futures') 

Futures %>%
  filter(Date > '2018-01-01') %>%
  ggplot(aes(Date, Close)) +
  geom_line(color = 'red', show.legend = TRUE) +
  geom_line(aes(Date, OldClose), color = 'blue') +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,1800,100))) +
  theme(axis.title = element_text(size = 28, family = 'Freight Black'),
        title = element_text(size = 30),
        axis.text = element_text(size = 24)) +
  labs(title = 'Amended Lumber Futures, 2022-Current', y = 'Close', subtitle = 'Blue = Old Futures, Red = New Futures') 

Futures %>%
  filter(Date > '2022-01-01') %>%
  ggplot(aes(Date, Close)) +
  geom_line(color = 'red', show.legend = TRUE) +
  geom_line(aes(Date, OldClose), color = 'blue') +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,1800,100))) +
  theme(axis.title = element_text(size = 28, family = 'Freight Black'),
        title = element_text(size = 30),
        axis.text = element_text(size = 24)) +
  labs(title = 'Amended Lumber Futures, 2022-Current', y = 'Close', subtitle = 'Blue = Old Futures, Red = New Futures') 
  
