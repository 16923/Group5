#This is a part of preliminary analysis
#this is to do logi-regression on asthma severity

data <- read.csv("dataseverity.csv")

head(data)

fit1_1 <- glm(asthma~FVC,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~FEV1,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~PEF,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~BMI,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~age,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~sex,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~WBC,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~neu,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))
fit1_1 <- glm(asthma~eos,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))


#check: association between duration and injection history
fit1_1 <- glm(asthma~age+sex+BMI+WBC+FVC,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~age+sex+BMI+WBC+FEV1,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~age+BMI+WBC+sex*FEV1,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

table(data$severity)

table(data$FEV1)

data$mild <- if_else(data$severity=="Mild Intermittent", 0, 1) 
data$severe <- if_else(data$severity=="Severe Persistant", 1, 0) 
table(data$mild)
fit1_1 <- glm(mild ~ sex+FEV1, family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(mild~age+BMI+WBC+sex*FEV1,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(severe~age+BMI+WBC+sex*FEV1,family = "binomial", data=data)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))


library(tableone)
myVars <- c("sex", "age","asthma", "agediag","FVC", "FEV1", "WBC", "eos", "PEF",
            "sBP", "BMI","med")
## Vector of categorical variables that need transformation
catVars <- c("sex", "asthma","med")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = data,strata = "severity")
a <- print(tab2, quote = FALSE, noSpaces = TRUE)

as.data.frame(a)
write.csv(x=a,file="tab1.csv")
library("FactoMineR")
library("factoextra")
library("tidyverse")
library("flexclust")

library(glmnet)




#select k=4
data$sex <- if_else(data$sex=="Male", 1, 0) 
data$med <- if_else(data$med=="Yes", 1, 0) 
#data <- data[,-(31:32)]

data1 <- data %>% select_if(is.numeric)
data1 <-data1[,-(17:22)]


# ????????????
clk<-kcca(data1,k=4)

clk@centers


summary(clk)

res <- kmeans(data1, centers=4)
pairs(data1, col = res$cluster + 1)



clk<-kcca(data1_0,k=4)

clk@centers


summary(clk)

res <- kmeans(data1_0, centers=4)
pairs(data1_0, col = res$cluster + 1)



clk<-kcca(data1_1,k=4)

clk@centers


summary(clk)

res <- kmeans(data1_1, centers=4)
pairs(data1_1, col = res$cluster + 1)



#run 3 times

library(GGally)
data1$severity <- data$severity
ggpairs(data1_1,columns = c(1,3,4,5),mapping=aes(colour=as.character(fit$cluster)))

ggpairs(data1_1,columns = 7:10,mapping=aes(colour=as.character(fit$cluster)))
ggpairs(data1_1,columns = 11:12,mapping=aes(colour=as.character(fit$cluster)))
ggpairs(data1_1,columns = 17:18,mapping=aes(colour=as.character(fit$cluster)))
data1_1$severity  <- data1$severity[data1$asthma ==1]

ggpairs(data1_1,columns = c(1,4,12,20),mapping=aes(colour=as.character(fit$cluster)))



data1_0$severity  <- data1$severity[data1$asthma ==0]

data1_0$severity  <- as.factor(data1_0$severity) 

  
data1_0$severity [data1_0$severity== "Mild Intermittent"] <- 1





library(flexclust)

clk2 <- cclust(data1_1, k=4)
barchart(clk2,legend=TRUE)



fviz_nbclust(data1_1, kmeans, method = "wss")


library(tidyverse)
data1_0 <- data1 %>% filter(asthma == 0)
data1_1 <- data1 %>% filter(asthma == 1)


fit<-kmeans(data1,4)

library(ggfortify)
library(ggplot2)

autoplot(kmeans(data1, 4),data=data1,label=TRUE, label.size=3, frame=TRUE)




library(ggfortify)
library(ggplot2)
data1_0<-data1_0[,-20]
fit<-kmeans(data1_1,4)
autoplot(kmeans(data1_1, 4),data=data1_1,label=TRUE, label.size=3, frame=TRUE)

res.pca <- PCA(data1_1, scale.unit = TRUE, graph = T)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


fit<-kmeans(data1_1,4)
autoplot(kmeans(data1_1, 4),data=data1_1,label=TRUE, label.size=3, frame=TRUE)




fit<-kmeans(data3,5)
autoplot(kmeans(data, 5),data=data,label=TRUE, label.size=3, frame=TRUE)
