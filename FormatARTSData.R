library(openxlsx)
library(tidyverse)
library(priceR)
library(conflicted)

Data <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/ARTS_Sales_Selected.xlsx")
Data <- Data %>%
  pivot_longer(cols = c(`2021`:`1992`), names_to = 'Year', values_to = 'Value') 
Data <- Data %>%
  pivot_wider(id_cols = c(Year), names_from = Variable, values_from = Value)
Data <- Data %>%  
  arrange(Year)

write.xlsx(Data,"C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/AHSProject/OutsideDataSources_SingleYearApprox/ARTS_Sales_Selected_Formatted.xlsx")
