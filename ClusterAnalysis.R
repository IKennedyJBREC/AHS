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
conflicts_prefer(dplyr::summarize(), dplyr::rename(), dplyr::group_by(), dplyr::filter(), dplyr::mutate(), base::as.numeric())


Data <- read.xlsx("C:/Users/ikennedy/OneDrive - JBREC/General - BP research_ public use microdata coding and data/CleanedData_Ian/Data_1995_2021.xlsx")

Data_2021 <- Data %>%
  mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
  filter(!is.na(JOBCOST) & AHSYEAR == 2021) %>%
  mutate(JOBCOST_SQRT = sqrt(JOBCOST),
         JOBCOST_LOG = log10(JOBCOST))

Data_2021$JOBCOSTScaled <- scale(Data_2021$JOBCOST)


sum(Data_2021$JOBCOST_WEIGHTED)
Clusters <-kmeans(Data_2021$JOBCOST_SQRT, centers = 4, iter.max = 1000, nstart = 1)

Clusters <- Clusters$cluster

Data_2021$Cluster <- Clusters

ClusterSummary <- Data_2021 %>%
  mutate(Cluster = case_when(JOBCOST < 10000 ~ 1,
                          JOBCOST >= 10000 & JOBCOST < 30000 ~ 2, 
                          JOBCOST >= 30000 ~ 3)) %>%
  group_by(Cluster) %>%
  summarize(Min = min(JOBCOST),
                      p05 = quantile(JOBCOST, .05),
                      p10 = quantile(JOBCOST, .1),
                      p25 = quantile(JOBCOST, .25),
                      Mean = mean(JOBCOST),
                      p75 = quantile(JOBCOST, .75),
                      p90 = quantile(JOBCOST, .9),
                      p95 = quantile(JOBCOST, .95),
                      Max = max(JOBCOST),
                      Count = n()) %>%
  arrange(Max)

Data_2021 %>%
  ggplot(aes(fct_reorder(as.factor(Cluster), JOBCOST), JOBCOST, color = as.factor(Cluster))) +
  geom_jitter(size = .1, alpha = .5, width = .4) +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,300000, 10000))) +
  theme_bw() +
  scale_color_discrete(type = c('orange', 'red', 'green', 'black')) +
  geom_hline(yintercept = ClusterSummary$Max[1], show.legend = FALSE, color = 'red') +
  geom_hline(yintercept = ClusterSummary$Max[2], show.legend = FALSE, color = 'orange') +
  geom_hline(yintercept = ClusterSummary$Max[3], show.legend = FALSE, color = 'green') +
  geom_hline(yintercept = ClusterSummary$Max[4], show.legend = FALSE, color = 'black') +
  labs(title = 'Job Cost Clusters - KMeans', x = 'Cluster', y = 'Cost') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none') +
  annotate("text", x=1, y=ClusterSummary$Max[1]+5000, label = as.character(ClusterSummary$Max[1])) +
  annotate("text", x=1, y=ClusterSummary$Max[1]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[1]))) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+5000, label = as.character(ClusterSummary$Max[2])) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[2]))) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+5000, label = as.character(ClusterSummary$Max[3])) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[3]))) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+5000, label = as.character(ClusterSummary$Max[4])) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[4]))) 



Data_2021 <- Data %>%
  mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
  filter(!is.na(JOBCOST) & AHSYEAR == 2021 & TENURE == 'Owned/Bought') %>%
  mutate(MAINTAMT = as.numeric(MAINTAMT), 
         TOTBALAMT = as.numeric(TOTBALAMT),
         JOBFUNDS = as.factor(JOBFUNDS),
         GUTREHB = as.factor(GUTREHB),,
         JOBCOMP = as.factor(JOBCOMP),
         JOBDIY = as.factor(JOBDIY),
         JobCategory = as.factor(JobCategory))

H2O_2021 <- Data_2021 %>%
  select(JobCategory, JOBDIY, JOBCOST)

Data_2021 <- Data_2021[sample(1:nrow(Data_2021), 10000,
                              replace=FALSE),]

Data_2021 <- Data_2021[,c("JOBCOST")]



Clara_dist <- clara(Data_2021, k = 4, metric = 'jaccard', stand = TRUE, cluster.only = TRUE)
Data_2021$Cluster <- Clara_dist


# Daisy Clustering
d_dist <- daisy(Data_2021, metric = 'gower', weights = c(1,2,3), type = list(numeric = 1, factor = c(2,3)), stand = TRUE)
d_dist <- daisy(as.data.frame(Data_2021), metric = 'gower', stand = TRUE)
# hierarchical clustering
hc <- hclust(d_dist, method = "complete")
# dendrogram 
plot(hc, labels=FALSE)
rect.hclust(hc, k=4, border="red")
# choose k, number of clusters 
cluster<-cutree(hc, k=4)
# add cluster to original data 
Data_2021$Cluster <- cluster
Data_2021 <- data.frame(JOBCOST = Data_2021,
                        Cluster = cluster)


# Prepare the Distance Matrix
Distances <- dist(Data_2021)
# Generate hclust for complete, single & average linkage methods
hc_complete <- hclust(Distances, method = "complete")
clusterCut <- cutree(hc_complete, 4)
Data_2021 <- Data %>%
  mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
  filter(!is.na(JOBCOST) & AHSYEAR == 2021)
Data_2021$Cluster <- clusterCut

ClusterSummary <- Data_2021 %>%
  group_by(Cluster) %>%
  summarize(Min = min(JOBCOST^2),
            Mean = mean(JOBCOST^2),
            Max = max(JOBCOST^2),
            Count = n())

ClusterSummary <- ClusterSummary %>%
  arrange(Max)

Data_2021 %>%
  ggplot(aes(fct_reorder(as.factor(Cluster), JOBCOST), JOBCOST, color = as.factor(Cluster))) +
  geom_jitter(size = .1, alpha = .5, width = .3) +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,300000, 10000))) +
  theme_bw() +
  scale_color_discrete(type = c('red', 'orange', 'green', 'black')) +
  geom_hline(yintercept = ClusterSummary$Max[1], show.legend = FALSE, color = 'red') +
  geom_hline(yintercept = ClusterSummary$Max[2], show.legend = FALSE, color = 'orange') +
  geom_hline(yintercept = ClusterSummary$Max[3], show.legend = FALSE, color = 'green') +
  geom_hline(yintercept = ClusterSummary$Max[4], show.legend = FALSE, color = 'black') +
  labs(title = 'Job Cost Clusters - Hierarchical', x = 'Cluster', y = 'Cost') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none') +
  annotate("text", x=1, y=ClusterSummary$Max[1]+5000, label = as.character(ClusterSummary$Max[1])) +
  annotate("text", x=1, y=ClusterSummary$Max[1]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[1]))) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+5000, label = as.character(ClusterSummary$Max[2])) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[2]))) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+5000, label = as.character(ClusterSummary$Max[3])) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[3]))) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+5000, label = as.character(ClusterSummary$Max[4])) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[4]))) 


# H2O Clustering
Data_2021 <- Data %>%
  mutate(JOBCOST = ifelse(JOBCOST == 0, NA, JOBCOST)) %>%
  filter(!is.na(JOBCOST) & AHSYEAR == 2021 & TENURE == 'Owned/Bought' & JobCategory != 'DisRepairs') %>%
  mutate(MAINTAMT = as.numeric(MAINTAMT), 
         TOTBALAMT = as.numeric(TOTBALAMT),
         JOBFUNDS = as.factor(JOBFUNDS),
         GUTREHB = as.factor(GUTREHB),
         JOBCOMP = as.factor(JOBCOMP),
         JOBDIY = as.factor(JOBDIY),
         JobCategory = as.factor(JobCategory),
         JOBCOST = sqrt(JOBCOST))

H2O_2021 <- Data_2021 %>%
  select(JobCategory, JOBDIY, JOBCOST, JOBFUNDS)

predictors <- names(H2O_2021)
h2o.init()
H2O_2021 <- as.h2o(H2O_2021)
Split <- h2o.splitFrame(data = H2O_2021, ratios = 0.8, seed = 436)
train <- Split[[1]]
valid <- Split[[2]]

# Build and train the model:
Cost_kmeans <- h2o.kmeans(k = 4,
                          estimate_k = FALSE,
                          standardize = FALSE,
                          max_iterations = 1000,
                          seed = 1234,
                          max_runtime_secs = 1000,
                          x = predictors,
                          training_frame = train,
                          validation_frame = valid)

# Eval performance:
perf <- h2o.performance(Cost_kmeans)

# Generate predictions on a validation set (if necessary):
ValidPreds <- as.data.frame(h2o.predict(Cost_kmeans, newdata = valid))
valid$Cluster <- ValidPreds$predict

# Generate predictions on a validation set (if necessary):
TrainingPreds <- as.data.frame(h2o.predict(Cost_kmeans, newdata = train))
train <- as.data.frame(train)
train$Cluster <- TrainingPreds$predict

ClusterSummary <- train %>%
  group_by(Cluster) %>%
  summarize(Min = min(JOBCOST),
            Mean = mean(JOBCOST),
            Max = max(JOBCOST),
            Count = n())

ClusterSummary <- train %>%
  group_by(Cluster) %>%
  summarize(Min = min(JOBCOST^2),
            p05 = quantile(JOBCOST^2, .05),
            p10 = quantile(JOBCOST^2, .1),
            p25 = quantile(JOBCOST^2, .25),
            Mean = mean(JOBCOST^2),
            p75 = quantile(JOBCOST^2, .75),
            p90 = quantile(JOBCOST^2, .9),
            p95 = quantile(JOBCOST^2, .95),
            Max = max(JOBCOST^2),
            Count = n())

ClusterSummary <- ClusterSummary %>%
  arrange(Max)

train %>%
  ggplot(aes(fct_reorder(as.factor(Cluster), JOBCOST^2), JOBCOST^2, color = as.factor(Cluster))) +
  geom_jitter(size = .1, alpha = .5, width = .3) +
  scale_y_continuous(labels = scales::comma, breaks = c(seq(0,300000, 10000))) +
  theme_bw() +
  scale_color_discrete(type = c('red', 'orange', 'green', 'black')) +
  geom_hline(yintercept = ClusterSummary$Max[1], show.legend = FALSE, color = 'red') +
  geom_hline(yintercept = ClusterSummary$Max[2], show.legend = FALSE, color = 'orange') +
  geom_hline(yintercept = ClusterSummary$Max[3], show.legend = FALSE, color = 'green') +
  geom_hline(yintercept = ClusterSummary$Max[4], show.legend = FALSE, color = 'black') +
  labs(title = 'Job Cost Clusters - Hierarchical', x = 'Cluster', y = 'Cost') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none') +
  annotate("text", x=1, y=ClusterSummary$Max[1]+5000, label = as.character(ClusterSummary$Max[1])) +
  annotate("text", x=1, y=ClusterSummary$Max[1]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[1]))) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+5000, label = as.character(ClusterSummary$Max[2])) +
  annotate("text", x=2, y=ClusterSummary$Max[2]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[2]))) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+5000, label = as.character(ClusterSummary$Max[3])) +
  annotate("text", x=3, y=ClusterSummary$Max[3]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[3]))) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+5000, label = as.character(ClusterSummary$Max[4])) +
  annotate("text", x=4, y=ClusterSummary$Max[4]+15000, label = paste0('SS: ', as.character(ClusterSummary$Count[4]))) 

