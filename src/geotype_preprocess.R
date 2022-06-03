#This is the code to parse and cluster the genotype PCs (not useful in the end)

library(tidyverse)

library(ggfortify)
library(ggplot2)

library("FactoMineR")
library("factoextra")
library("tidyverse")
library("flexclust")


# -- finished part

data <- readRDS("ukb_extracted_genotype.rds")

head(data)

data_neat<-data[,-c(3,5:7,9:18,60:82,86:88,90:91,93:97)]



data_asthma <- subset(data_neat,data_neat$'Doctor diagnosed asthma.0.0'==1) 

data_valid <- subset(data_neat,data_neat$'Doctor diagnosed asthma.0.0'==0 | data_neat$'Doctor diagnosed asthma.0.0'==1)


write_csv(data_asthma,"data_asthma.csv")
write_csv(data_valid,"data_valid.csv")



# things need to be done:
data_asthma <- read_csv("data_asthma.csv")
data_valid <- read_csv("data_valid.csv")


#read_in data_valid and data_asthma


# exclude bad quality records


#drop NA rows

#check all the columns are numeric

data_asthma2<- data_asthma[,-c(1:4,45)]
#data_asthma2<- data_asthma[,-42]
colSums(is.na(data_asthma2))
data_asthma2<- na.omit(data_asthma2)

data_asthma3<-data_asthma2[,-42]

#if error occurs in k-means, adjust the following line or drop any knotty columns if needed
data_asthma3 <- data_asthma3 %>% select_if(is.numeric)

#best cluster
fviz_nbclust(data_asthma3, kmeans, method = "wss")

#fit kmeans
fit<-kmeans(data_asthma3,3)

#plot
autoplot(kmeans(data_asthma3, 3),data=data_asthma3,label=TRUE, label.size=3, frame=TRUE)
data_asthma2$cluster <-fit$cluster

#library(glmnet)



#plot the barplot
clk2 <- cclust(data_asthma3, k=3)
barchart(clk2,legend=TRUE)

library(GGally)

ggpairs(data_asthma2,columns = c(1,42,51),mapping=aes(colour=as.character(data_asthma2$cluster)))

c1 <- subset(data_asthma2,data_asthma2$cluster==3) 
c2 <- subset(data_asthma2,data_asthma2$cluster==2) 

  

