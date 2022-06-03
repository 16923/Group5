#This is the data preliminary analysis
#including imputation of the data, k-means clustering
#and non-penalised regression between exposures and asthma

rm(list=ls())
getwd()
#data <- readRDS("../outputs/ukb_recoded.rds")

#saveRDS(data, "../outputs/ukb_recoded_neat_v1.rds")
data <- readRDS("../outputs/ukb_recoded_neat_v1.rds")

head(data)


table(data$'Doctor diagnosed asthma.0.0')

library("FactoMineR")
library("factoextra")
library("tidyverse")
library("flexclust")

data1 <- data %>% select_if(is.numeric)

res.pca <- PCA(data1, scale.unit = TRUE, graph = T)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Contributions of variables to PC1

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2

fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

data2=data.frame(data[,60],data1[,(1:43)])
colnames(data2)[1] <- 'asthma'         

data2<- filter(data2,!is.na(data2$asthma))
Y<- data2[,1]
X <- data2[,-1]

library(glmnet)

write.csv(x = data2,file = "asthmaV3.csv")

library(tidyverse)
data2$asthma <- if_else(data2$asthma=="Yes", 1, 0) 
data2$age <- 2022-data2$'Year.of.birth.0.0'


fit1_1 <- glm(asthma~age,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))
fit1_1 <- glm(asthma~FEV1,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

data2$FVC <- rowMeans(data2[3:5], na.rm=TRUE)


data2$FEV1 <- rowMeans(data2[6:8], na.rm=TRUE)

data2$PEF <- rowMeans(data2[9:11], na.rm=TRUE)

data2$dBP <- rowMeans(data2[12:13], na.rm=TRUE)

data2$sBP <- rowMeans(data2[14:15], na.rm=TRUE)
data2$BMI <- rowMeans(data2[22], na.rm=TRUE)

fit1_1 <- glm(asthma~FVC,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~FEV1,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~PEF,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))
fit1_1 <- glm(asthma~dBP,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))
fit1_1 <- glm(asthma~sBP,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))
fit1_1 <- glm(asthma~BMI,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

data2$WBC <- rowMeans(data2[39], na.rm=TRUE)
data2$neu<- rowMeans(data2[42], na.rm=TRUE)
data2$eos <- rowMeans(data2[43], na.rm=TRUE)


fit1_1 <- glm(asthma~WBC,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~neu,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

fit1_1 <- glm(asthma~eos,family = "binomial", data=data2)
summary(fit1_1)
exp(cbind(coef(fit1_1), confint(fit1_1)))

data_neat = data_valid %>%filter(is.na(wellbeing1yr)==FALSE)
data_neat$intervention <- relevel(data_neat$intervention,"standard of care")
data_split$intervention <- relevel(data_split$intervention,"standard of care")



fit3_2 <- lm(wellbeing1yr~intervention,data=data_neat)
summary(fit3_2)
confint(fit3_2)

par(mfrow=c(2,2))
plot(fit3_1)



data2$onset <- if_else(data2$asthma=="Yes", 1, 0) 



set.seed(1551)

folds <- lapply(split(sample(seq(1, nrow(X), by=2)), rep(1:5, length=length(Y)/2)), FUN=function(x){return(c(x, (x+1)))})

fold_ids <- c(rep(1,length(folds$`1`)),rep(2,length(folds$`2`)),rep(3,length(folds$`3`)),
              rep(4,length(folds$`4`)),rep(5,length(folds$`5`)))
ids <- c(folds$`1`,folds$`2`,folds$`3`,folds$`4`,folds$`5`)
fold_data <- data.frame(fold_ids,ids)
fold_data <- fold_data[order(fold_data$ids),]

cv_logistic_auc <- cv.glmnet(X, Y, family = "binomial", type.measure = "auc", foldid = fold_data$fold_ids)

plot(cv_logistic_auc)

cv_logistic_auc$lambda.1se

coefficients_logit_1se <- coef(cv_logistic_auc, s=cv_logistic_auc$lambda.1se)

non_zero_index <- coefficients_logit_1se@i

non_zero_coefficients <- coefficients_logit_1se@x

cbind(c("Intercept",colnames(proteins_denoised)[non_zero_index]),non_zero_coefficients)


data3<- subset(data2, data2$asthma=="Yes") 
data3 <- data3[,-1]



#select k=5
fviz_nbclust(data3, kmeans, method = "wss")


fit<-kmeans(data3,4)

library(ggfortify)
library(ggplot2)

autoplot(kmeans(data3, 4),data=data3,label=TRUE, label.size=3, frame=TRUE)

fit<-kmeans(data3,5)
autoplot(kmeans(data3, 5),data=data3,label=TRUE, label.size=3, frame=TRUE)



#imp

#Missing Type Determination: MAR
data_neat = data2[,-(47:49)]
data_neat = data_neat[,-(25:43)]
data_neat = data_neat[,-(1:15)]
data_neat = data_neat[,-(14:16)]
data_neat = data_neat[,-(7:8)]
library(VIM)
aggr(data_neat, prop = F, numbers = T,labels = names(data2), cex.axis = .9) 

data_neat$FEV1_best <- data_neat[,1]
data_neat$FVC_best <- data_neat[,2]
data_neat$FEV1_pred <- data_neat[,3]
data_neat$FEV1_FVC_ratio <- data_neat[,6]
data_neat <- data_neat[,-(1:6)]

library(mice)
marginplot(data_neat[,c("FEV1_best","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FVC_best","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FEV1_pred","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FEV1_FVC_ratio","asthma")],col = mdc(1:2),cex.numbers=1.2,pch=19)


marginplot(data_neat[,c("FEV1","FEV1_best")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FVC","FVC_best")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FEV1","FEV1_pred")],col = mdc(1:2),cex.numbers=1.2,pch=19)
marginplot(data_neat[,c("FEV1","FEV1_FVC_ratio")],col = mdc(1:2),cex.numbers=1.2,pch=19)


library(corrplot)

x<-as.data.frame(abs(is.na(data_neat)))
y<-x[which(apply(x,2,sum)>0)]

data_num <- data_neat[,-(1:8)]
x<-as.data.frame(abs(is.na(data_num)))
y<-x[which(apply(x,2,sum)>0)]

corp <- cor(y)
corp
#cor(data_num,y,use="everything")
#cor(data_num,y,use="pairwise.complete.obs")

corrplot(corp,method="pie",type="lower")

#mice MCMC imputation
imp <- mice(data_neat,m=5,printFlag=FALSE,maxit=40,seed=123)
#head(imp)

#two exposure variables chosen according to LR test in #supplementary
fit.mi <- with(data=imp, exp = lm(FEV1_best ~ FEV1 + PEF + FVC))

combFit <- pool(fit.mi)
summary(combFit)

pool.r.squared(fit.mi)

densityplot(imp)

stripplot(imp, pch = 20, cex = 1.2)

data_imp2 <-complete(imp,action=2)
data_imp2 <-data_imp2[,(7:10)]

data_new <- cbind(data_imp,data_imp2)

data_temp <- subset(data,`Age asthma diagnosed by doctor.0.0` >= 0)
sex <- data_temp[,1]
data_new <- cbind(sex,data_new)
write.csv(x = data_new,file = "asthma_imputed.csv")
