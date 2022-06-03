#this is the k-prototype clustering to select asthma subtypes

library(tidyverse)

library(ggfortify)
library(ggplot2)

library("FactoMineR")
library("factoextra")
library("tidyverse")
library("flexclust")

data1_1 <- read.csv("data1-1.csv")
data1_2 <- read.csv("data1-2-2.csv")

data2_1 <- read.csv("data2-1.csv")
data2_2 <- read.csv("data2-2.csv")

data3_1 <- read.csv("data3-1.csv")
data3_2 <- read.csv("data3-2.csv")

data4_1 <- read.csv("data4-1.csv")
data4_2 <- read.csv("data4-2.csv")

data<-data3_1
data$Treatment.medication.code.0.0 <-as.factor(data$Treatment.medication.code.0.0)
data$Ethnic.background.0.0 <-as.factor(data$Ethnic.background.0.0)
data$Job.Type.0.0 <- as.factor(data$Job.Type.0.0)
data$Job.Type.0.1 <- as.factor(data$Job.Type.0.1)

library(clustMixType)



library(Hmisc)
data$Job.Type.0.0<-with(data,impute(data$Job.Type.0.0,"random"))
data$Job.Type.0.1<-with(data,impute(data$Job.Type.0.1,"random"))
data$Treatment.medication.code.0.0<-with(data,impute(data$Treatment.medication.code.0.0,"random"))


data <- data[complete.cases(data),]

data_decide<- data %>% select_if(is.numeric)


# Elbow method
fviz_nbclust(data_decide, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(data_decide, kmeans, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Silhouette method")
fviz_nbclust(data_decide, kmeans, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Gapstat method")



library (cluster)
library (vegan)
# Silhouette method
silhouette_score <- function(k){
  km <- kmeans(data_decide, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data_decide))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


data<-data[,-c(31,56)]
data <- data[complete.cases(data),]
kpres <- kproto(data,3)
table(kpres$cluster)
data$cluster <-kpres$cluster
write.csv(data,"4_1_with_cluster.csv")

clprofiles(kpres, data)




col_names <- sapply(data,function(col) length(unique(col)) < 4)
data[,col_names] <- lapply(data[,col_names],factor)
colSums(is.na(data))


# in real world clusters are often not as clear cut
# by variation of lambda the emphasize is shifted towards factor / numeric variables
data$cluster <-kpres$cluster
data <-data[,-72]

#if error occurs in k-means, adjust the following line or drop any knotty columns if needed
data <- data %>% select_if(is.numeric)

fit<-kmeans(data_decide,3)

#plot
autoplot(kmeans(data_decide, 3),data=data_decide,label=TRUE, label.size=3, frame=TRUE)

library(cclust)
#plot the barplot
clk2 <- cclust(data_decide,k=3)
barchart(clk2,legend=TRUE)

