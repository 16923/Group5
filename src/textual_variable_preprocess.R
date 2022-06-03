
#This is to incorporate multiple deprivation scores, 
#regional information, and job information to the main data

library(tidyverse)

library(ggfortify)
library(ggplot2)

library("FactoMineR")
library("factoextra")
library("tidyverse")
library("flexclust")

# -- finished part
#create data1


data <- readRDS("ukb_extracted_text.rds")

data0 <- read.csv("asthmaV3.csv")



data_neat<-data[,-c(3,5:7,9:11,13:15,17:19,21:23,25:27,29:31,33:35,37:39,41:42,44:45,47:49,51:241,246:247,249:251,290:327,329:367,369:407,409:567,569:642,644:665,695:764,771:948,955:1174)]

data_neat <- subset(data_neat,is.na(data_neat$'Doctor diagnosed asthma.0.0') ==FALSE) 


data0 <- data0[,-c(1,3,25:40)]

#preprocess data0

library("tgutil")
library(dplyr)
# Make min and max columns for variance calculations of PEF
data0 <- transform(data0, minPEF = pmin(Peak.expiratory.flow..PEF..0.0, Peak.expiratory.flow..PEF..0.1, Peak.expiratory.flow..PEF..0.2, na.rm = TRUE))
data0 <- transform(data0, maxPEF = pmax(Peak.expiratory.flow..PEF..0.0, Peak.expiratory.flow..PEF..0.1, Peak.expiratory.flow..PEF..0.2, na.rm = TRUE))
data0 <- transform(data0, FEV = pmean(Forced.expiratory.volume.in.1.second..FEV1..0.0, Forced.expiratory.volume.in.1.second..FEV1..0.1, Forced.expiratory.volume.in.1.second..FEV1..0.2, na.rm = TRUE))
data0 <- transform(data0, FVC = pmean(Forced.vital.capacity..FVC..0.0, Forced.vital.capacity..FVC..0.1, Forced.vital.capacity..FVC..0.2, na.rm = TRUE))
data0 <- transform(data0, PEF = pmean(Peak.expiratory.flow..PEF..0.0, Peak.expiratory.flow..PEF..0.1, Peak.expiratory.flow..PEF..0.2, na.rm = TRUE))
data0 <- data0 %>% mutate(varPEF = (100*(maxPEF - minPEF)) / (maxPEF) )
data0 <- transform(data0, dBP = pmean(Diastolic.blood.pressure..automated.reading.0.0, Diastolic.blood.pressure..automated.reading.0.1, na.rm = TRUE))
data0 <- transform(data0, sBP = pmean(Systolic.blood.pressure..automated.reading.0.0, Systolic.blood.pressure..automated.reading.0.1, na.rm = TRUE))


data0$FEV1_best <- data0[,16]
data0$FVC_best <- data0[,17]
data0$FEV1_pred <- data0[,18]
data0$FEV1_pred_precentage <- data0[,19]
data0$FVC_Z <- data0[,20]
data0$FEV1_FVC_ratio_Z <- data0[,21]

data0 <- data0[,-c(3:21)]

datafull_newest <- cbind(data0, data_neat)
colSums(is.na(datafull_newest))

datafull_newest <- transform(datafull_newest, income = pmax(datafull_newest[,87], datafull_newest[,94],datafull_newest[,104], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, health = pmax(datafull_newest[,89], datafull_newest[,96],datafull_newest[,105], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, education = pmax(datafull_newest[,90], datafull_newest[,97],datafull_newest[,106], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, housing = pmax(datafull_newest[,91], datafull_newest[,99],datafull_newest[,107], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, crime = pmax(datafull_newest[,92], datafull_newest[,101],datafull_newest[,109], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, deprivation = pmax(datafull_newest[,86], datafull_newest[,102],datafull_newest[,103],na.rm = TRUE))

datafull_newest <- datafull_newest %>%  mutate(England = if_else(is.na(datafull_newest[,86]) == FALSE,1,0),
                                               Scotland = if_else(is.na(datafull_newest[,102]) == FALSE,1,0),
                                               Wales = if_else(is.na(datafull_newest[,103]) == FALSE,1,0))

datafull_newest<-datafull_newest[,-c(85:109)]
datafull_newest<-datafull_newest[,-c(85:89)]
datafull_newest$Treatment.medication.code.0.0 <- substr(datafull_newest$Treatment.medication.code.0.0,1,4)
datafull_newest$Job.Type.0.0<- substr(datafull_newest$Job.Type.0.0,1,3)
datafull_newest$Job.Type.0.1<- substr(datafull_newest$Job.Type.0.1,1,3)


write_csv(datafull_newest,"data1-1_temp.csv")

#go to python for phecode then manually change file

datafull_newest<-read.csv("icd_embedded_temp.csv")


datafull_newest<-datafull_newest[,-c(85:96)]
colSums(is.na(datafull_newest))
datafull_newest_for_imputation<-datafull_newest[,-c(61:78)]

colSums(is.na(datafull_newest_for_imputation))

write_csv(datafull_newest,"data1-2.csv")
write_csv(datafull_newest_for_imputation,"data1-2-2.csv")

datafull_newest_for_imputation_no_age_asthma <-datafull_newest_for_imputation[,-c(36)]

#imputation for data 1-1

library(VIM)
aggr(datafull_newest_for_imputation_no_age_asthma, prop = F, numbers = T,labels = names(datafull_newest_for_imputation_no_age_asthma), cex.axis = .9) 

library(mice)
#marginplot(data_new[,c("FVC","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
#marginplot(data_new[,c("FEV1","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
#marginplot(data_new[,c("PEF","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)

library(corrplot)

x<-as.data.frame(abs(is.na(datafull_newest_for_imputation_no_age_asthma)))
y<-x[which(apply(x,2,sum)>0)]

data_num <- data_new[,-(1:8)]
x<-as.data.frame(abs(is.na(data_num)))
y<-x[which(apply(x,2,sum)>0)]

corp <- cor(y)
corp
#cor(data_num,y,use="everything")
#cor(data_num,y,use="pairwise.complete.obs")

corrplot(corp,method="pie",type="lower")

#mice MCMC imputation
imp <- mice(datafull_newest_for_imputation_no_age_asthma,m=5,printFlag=FALSE,maxit=40,seed=123)
#head(imp)

#two exposure variables chosen according to LR test in #supplementary
fit.mi <- with(data=imp, exp = lm(FVC ~ FEV1 + PEF))

combFit <- pool(fit.mi)
summary(combFit)

pool.r.squared(fit.mi)

densityplot(imp)

stripplot(imp, pch = 20, cex = 1.2)

data_imp <-complete(imp,action=2)
colSums(is.na(data_imp))
write_csv(data_imp,"data1-1.csv")
#create data1 end

#run the normal process below this line
data <- readRDS("ukb_extracted_text.rds")

data0 <- read.csv("asthma_merged.csv")


data_neat<-data[,-c(3,5:7,9:11,13:15,17:19,21:23,25:27,29:31,33:35,37:39,41:42,44:45,47:49,51:241,246:247,249:251,290:327,329:367,369:407,409:567,569:642,644:665,695:764,771:948,955:1174)]

data_neat <- subset(data_neat,data_neat$'Doctor diagnosed asthma.0.0'==1) 



data <- readRDS("ukb_extracted_text.rds")

data0 <- read.csv("asthma_merged.csv")


data_neat<-data[,-c(3,5:7,9:11,13:15,17:19,21:23,25:27,29:31,33:35,37:39,41:42,44:45,47:49,51:241,246:247,249:251,290:327,329:367,369:407,409:567,569:642,644:665,695:764,771:948,955:1174)]


data_prev <- read.csv("asthmaV3.csv")


data_neat <- subset(data_neat,data_neat$'Doctor diagnosed asthma.0.0'==1) 



datafull_newest <- cbind(data0, data_neat)

datafull_newest<-datafull_newest[,-c(1:2,45:46,63,65:78)]

colSums(is.na(datafull_newest))
write_csv(datafull_newest,"data_20220318_raw.csv")
datafull_newest <- transform(datafull_newest, deprivation = pmax(datafull_newest[,69], datafull_newest[,85],datafull_newest[,86],na.rm = TRUE))


datafull_newest <- transform(datafull_newest, income = pmax(datafull_newest[,70], datafull_newest[,77],datafull_newest[,87], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, health = pmax(datafull_newest[,72], datafull_newest[,79],datafull_newest[,88], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, education = pmax(datafull_newest[,73], datafull_newest[,80],datafull_newest[,89], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, housing = pmax(datafull_newest[,74], datafull_newest[,82],datafull_newest[,90], na.rm = TRUE))
datafull_newest <- transform(datafull_newest, crime = pmax(datafull_newest[,75], datafull_newest[,84],datafull_newest[,90], na.rm = TRUE))

datafull_newest <- datafull_newest %>%  mutate(England = if_else(is.na(datafull_newest[,72]) == FALSE,1,0),
                                               Scotland = if_else(is.na(datafull_newest[,79]) == FALSE,1,0),
                                               Wales = if_else(is.na(datafull_newest[,88]) == FALSE,1,0))

datafull_newest<-datafull_newest[,-c(24,36,69:92)]


datafull_newest<-datafull_newest[,-c(67:71)]


colSums(is.na(datafull_newest))


library(tidyverse)

write_csv(datafull_newest,"data_20220318.csv")



#next part


data_new <- read.csv("icd_embedded2.csv")

data_new <- data_new[,-c(67:78)]

colSums(is.na(data_new))

data_t <- read.csv("UpdatedWithSeverity.csv")
data_new$severity <- data_t$severity

data_new <- data_new[,-c(59,66)]
colSums(is.na(data_new))
library(tidyverse)
write_csv(data_new,"data_20220324_with_icd2.csv")

#remove stress and other

data_new <- data_new[,-c(16:18)]
data_new$Treatment.medication.code.0.0 <- substr(data_new$Treatment.medication.code.0.0,1,4)
data_new$Job.Type.0.0<- substr(data_new$Job.Type.0.0,1,3)
data_new$Job.Type.0.1<- substr(data_new$Job.Type.0.1,1,3)



#imputation for data 2-1

library(VIM)
aggr(data_new, prop = F, numbers = T,labels = names(data_new), cex.axis = .9) 

library(mice)
#marginplot(data_new[,c("FVC","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
#marginplot(data_new[,c("FEV1","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
#marginplot(data_new[,c("PEF","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)

library(corrplot)

x<-as.data.frame(abs(is.na(data_new)))
y<-x[which(apply(x,2,sum)>0)]

data_num <- data_new[,-(1:8)]
x<-as.data.frame(abs(is.na(data_num)))
y<-x[which(apply(x,2,sum)>0)]

corp <- cor(y)
corp
#cor(data_num,y,use="everything")
#cor(data_num,y,use="pairwise.complete.obs")

corrplot(corp,method="pie",type="lower")

#mice MCMC imputation
imp <- mice(data_new,m=5,printFlag=FALSE,maxit=40,seed=123)
#head(imp)

#two exposure variables chosen according to LR test in #supplementary
fit.mi <- with(data=imp, exp = lm(FVC ~ FEV1 + PEF))

combFit <- pool(fit.mi)
summary(combFit)

pool.r.squared(fit.mi)

densityplot(imp)

stripplot(imp, pch = 20, cex = 1.2)

data_imp <-complete(imp,action=2)
colSums(is.na(data_imp))

write_csv(data_imp,"data2-1.csv")
c1 <- subset(data_imp,data_imp$asthma==0) 
c2 <- subset(data_imp,data_imp$asthma==1) 
write_csv(c1,"data3-1.csv")
write_csv(c2,"data4-1.csv")

colSums(is.na(data_new))

data_cc <-data_new[,-c(16:18,32,34,35,57)]
data_cc <- data_cc[complete.cases(data_new), ]

write_csv(data_cc,"data2-2.csv")
c1 <- subset(data_cc,data_cc$asthma==0) 
c2 <- subset(data_cc,data_cc$asthma==1) 
write_csv(c1,"data3-2.csv")
write_csv(c2,"data4-2.csv")

data_decide<- data_cc %>% select_if(is.numeric)

# Elbow method
fviz_nbclust(data_decide, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

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


# Gap Statistic
library(cluster)
set.seed(123)

gap_stat = clusGap(data_decide, FUN=kmeans, nstart=25, K.max=10, B=50)

fviz_gap_stat(gap_stat)

c1 <- subset(data_decide,data_cc$asthma==0) 
c2 <- subset(data_decide,data_cc$asthma==1) 


#if error occurs in k-means, adjust the following line or drop any knotty columns if needed
data <- data %>% select_if(is.numeric)

fit<-kmeans(data_decide,4)

#plot
autoplot(kmeans(data1_1, 4),data=data1_1,label=TRUE, label.size=3, frame=TRUE)

# apply k-prototyps
library(clustMixType)
data<-complete.cases(data)
kpres <- kproto(data,3)

clprofiles(kpres, x)
# in real world clusters are often not as clear cut
# by variation of lambda the emphasize is shifted towards factor / numeric variables
kpres <- kproto(x, 2)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 0.1)
clprofiles(kpres, x)
kpres <- kproto(x, 2, lambda = 25)
clprofiles(kpres, x)

library(cclust)
#plot the barplot
clk2 <- cclust(data_decide, k=3)
barchart(clk2,legend=TRUE)



#icd=0
library(tidyverse)

data_icd0 <- subset(data_cc,data_cc$icd_asthma==0) 

