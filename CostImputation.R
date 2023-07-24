library(ggthemes)
library(survey)
library(openxlsx)
library(tidyverse)
library(add2ggplot)
library(priceR)
library(sysfonts)
library(ClusterR)
library(cluster)
library(h2o)

rmse <- function (y_pred, y_true) {
  RMSE <- sqrt(mean((y_true - y_pred)^2))
  return(RMSE)
}

Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

# H2O Clustering
Data_2021 <- Data %>%
  mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
  filter(!is.na(JOBCOST) & AHSYEAR == 2021 & TENURE == 'Owned/Bought') %>%
  mutate(MAINTAMT = as.numeric(MAINTAMT), 
         TOTBALAMT = as.numeric(TOTBALAMT),
         JOBFUNDS = as.factor(JOBFUNDS),
         GUTREHB = if_else(GUTREHB == 'NR', NA, GUTREHB),
         GUTREHB = as.factor(GUTREHB),
         JOBCOMP = as.factor(JOBCOMP),
         JOBDIY = as.factor(JOBDIY),
         JobCategory = as.factor(JobCategory),
         JOBCOST = sqrt(JOBCOST),
         BLD = as.factor(BLD),
         DIVISION = as.factor(DIVISION),
         CSA = as.factor(CSA))

Data_2021_JB <- Data_2021 %>%
  select(-c(JOBCOST, DIVISION, REMODAMT, JOBDIY, JobCategory))

Data_2021 <- Data_2021 %>%
  select(JOBCOST, DIVISION, REMODAMT, JOBDIY, JobCategory)

MissingCosts <- Data %>%
  filter(AHSYEAR == 2021 & (is.na(JOBCOST) | JOBCOST == 0) & TENURE == 'Owned/Bought' & !is.na(JobCategory)) %>%
  mutate(JOBCOST = ifelse(!is.na(JOBCOST), NA, JOBCOST))

MissingCosts_JB <- MissingCosts %>%
  select(-c(JOBCOST, DIVISION, REMODAMT, JOBDIY, JobCategory))

MissingCosts <- MissingCosts %>%
  select(JOBCOST, DIVISION, REMODAMT, JOBDIY, JobCategory)

h2o.init()
H2OData <- as.h2o(Data_2021)

y <- "JOBCOST"
x <- names(Data_2021[,c(2:4)])

# Split data into train & validation sets
Splits <- h2o.splitFrame(H2OData, seed = 45, ratios = .8)
train <- Splits[[1]]
valid <- Splits[[2]]

# Run automatic machine learning
automl_model <- h2o.automl(x = x,
                           y = y,
                           training_frame = train,
                           max_models = 1000,
                           exclude_algos = 'DeepLearning',
                           sort_metric = "RMSE",
                           stopping_metric = "RMSE",
                           stopping_tolerance = .05,
                           max_runtime_secs = 300,
                           validation_frame = valid,
                           leaderboard_frame = valid,
                           #preprocessing = list("target_encoding"),
                           nfolds = 5,
                           seed = 45)

lb <- automl_model@leaderboard
leader <- automl_model@leader
lb
summary(leader)

PredVar <- eply::unquote(y)

VI <- h2o.varimp(leader)

VI <- VI %>%
  top_n(scaled_importance, n = 4) %>%
  mutate(variable = as.factor(variable),
         variable = fct_reorder(variable, scaled_importance)) %>%
  filter(scaled_importance > 0)


VI %>% 
  ggplot(aes(y = fct_reorder(variable, scaled_importance), x = scaled_importance)) +
  geom_col() +
  scale_x_continuous(breaks = c(seq(0,1,.1)), limits = c(0,1)) +
  labs(title = "h2o Regression - Scaled Variable Importance Scores", subtitle = "Scores calculated using test data.", y = "Scaled VI Score", x = "Variable") +
  theme_fivethirtyeight(base_size = 20, base_family = 'serif') +
  theme(axis.title = element_text(family = 'serif', size = 20))

testpreds <- h2o.predict(leader, valid) %>%
  as.data.frame()



TestData <- as.data.frame(valid) %>%
  cbind(testpreds) %>%
  mutate(predict = predict^2, 
         JOBCOST = JOBCOST^2) %>%
  mutate(Res = abs(predict - JOBCOST)) 

median(TestData$Res)

GroupedRes <- TestData %>%
  group_by(JobCategory) %>%
  summarise_at(vars(Res), list(MeanRes = mean))

GroupedRes2 <- TestData %>%
  dplyr::group_by(JobCategory) %>%
  dplyr::summarize(rmse = rmse(predict, JOBCOST))

GroupedRes <- GroupedRes %>%
  left_join(GroupedRes2, by = 'JobCategory') %>%
  mutate(MeanRes = round(MeanRes, 2),
         rmse = round(rmse, 2))
rm(test2)

max(TestData$predict)
TestData %>%
  ggplot(aes(sqrt(predict), sqrt(JOBCOST))) +
  geom_point(size = 1) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 300), breaks = c(seq(0,300,25))) +
  scale_y_continuous(limits = c(0, 300), breaks = c(seq(0,300, 25))) +
  facet_wrap(~JobCategory, nrow = 2) +
  theme_fivethirtyeight(base_size = 20, base_family = "serif") +
  theme(axis.title = element_text(family = 'serif', size = 30), axis.title.x = element_text(family = 'serif', size = 30), 
        axis.title.y = element_text(family = 'serif', size = 30)) +
  labs(title = "h2o Regression - Test Data Residual Plot", 
       subtitle = "SqRt(Job Cost)" , x = "Prediction", y = "Observation") +
  geom_text(x = 280, y = 20, aes(label = round(sqrt(MeanRes),2), size = 1), data = GroupedRes) +
  annotate("text", x = 180, y = 20, label = "Mean Res: ") +
  geom_text(x = 275, y = 50, aes(label = round(sqrt(rmse),2), size = 1), data = GroupedRes) +
  annotate("text", x = 200, y = 50, label = "RMSE: ") +
  theme(legend.position="none") +
  ggpubr::stat_regline_equation(label.x = 175, label.y = 280, aes(label = ..rr.label..))
