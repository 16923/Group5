# Setup -------------------------------------------------------------------

library(randomForest); library(dplyr); library("readxl"); library(caret)

data1_1 <- read_excel("data1-1.xlsx")
data1_2 <- read_excel("data1-2-2.xlsx")
data2_1 <- read.csv('data2-1.csv')
data2_2 <- read.csv('data2-2.csv')
data3_1 <- read.csv('data3-1.csv')
data3_2 <- read.csv('data3-2.csv')
data4_1 <- read.csv('data4-1.csv')
data4_2 <- read.csv('data4-2.csv')


dim(data2_2)
names(data1_1)
names(data4_2)#36

col_names <- sapply(data1_1, function(col) length(unique(col)) < 4)
data1_1[ , col_names] <- lapply(data1_1[ , col_names] , factor)
col_names <- sapply(data2_1, function(col) length(unique(col)) < 4)
data2_1[ , col_names] <- lapply(data2_1[ , col_names] , factor)
col_names <- sapply(data3_1, function(col) length(unique(col)) < 4)
data3_1[ , col_names] <- lapply(data3_1[ , col_names] , factor)
col_names <- sapply(data4_1, function(col) length(unique(col)) < 4)
data4_1[ , col_names] <- lapply(data4_1[ , col_names] , factor)
col_names <- sapply(data1_2, function(col) length(unique(col)) < 4)
data1_2[ , col_names] <- lapply(data1_2[ , col_names] , factor)
col_names <- sapply(data2_2, function(col) length(unique(col)) < 4)
data2_2[ , col_names] <- lapply(data2_2[ , col_names] , factor)
col_names <- sapply(data3_2, function(col) length(unique(col)) < 4)
data3_2[ , col_names] <- lapply(data3_2[ , col_names] , factor)
col_names <- sapply(data4_2, function(col) length(unique(col)) < 4)
data4_2[ , col_names] <- lapply(data4_2[ , col_names] , factor)

data1_1 <- data1_1 %>% select(where(~ n_distinct(.) > 1))
data2_1 <- data2_1 %>% select(where(~ n_distinct(.) > 1))
data3_1 <- data3_1 %>% select(where(~ n_distinct(.) > 1))
data4_1 <- data4_1 %>% select(where(~ n_distinct(.) > 1))
data1_2 <- data1_2 %>% select(where(~ n_distinct(.) > 1))
data2_2 <- data2_2 %>% select(where(~ n_distinct(.) > 1))
data3_2 <- data3_2 %>% select(where(~ n_distinct(.) > 1))
data4_2 <- data4_2 %>% select(where(~ n_distinct(.) > 1))

set.seed(101)
smp_size <- floor(1 * nrow(data1_1))
train_ind <- sample(seq_len(nrow(data1_1)), size = smp_size)


# Asthma Random Forests ----------------------------------------------------
yourSample <- data1_1 %>%
  group_by(Doctor.diagnosed.asthma.0.0) %>%
  sample_n(size = 15491) # currently buggy

yourSample2 <- data1_2 %>%
  group_by(Doctor.diagnosed.asthma.0.0) %>%
  sample_n(size = 15491) # currently buggy

rf_onset_111 <- randomForest(Doctor.diagnosed.asthma.0.0 ~ ., data = yourSample[,-c(1,2,75,76,77,78)],  na.action=na.exclude)
rf_onset_121 <- randomForest(Doctor.diagnosed.asthma.0.0 ~ ., data = yourSample2[,-c(1,2,36,76,77,78,79)],  na.action=na.exclude)

varImpPlot(rf_onset_111, main = 'Predicting asthma in cases and controls (with imputations)',cex.main = 0.7)
varImpPlot(rf_onset_121, main = 'Predicting asthma in cases and controls (no imputations)',cex.main = 0.7)


# Onset Random Forests ----------------------------------------------------
yourSample <- data2_1 %>%
  group_by(asthma) %>%
  sample_n(size = 3590) # currently buggy

yourSample2 <- data2_2 %>%
  group_by(asthma) %>%
  sample_n(size = 1435) # currently buggy

rf_onset_211 <- randomForest(asthma ~ .-Age.asthma.diagnosed.by.doctor.0.0, data = yourSample,  na.action=na.exclude)
rf_onset_221 <- randomForest(asthma ~ .-Age.asthma.diagnosed.by.doctor.0.0, data = yourSample2, na.action=na.exclude)

varImpPlot(rf_onset_211, main = 'Predicting onset in cases (with imputations)',cex.main = 0.7)
varImpPlot(rf_onset_221, main = 'Predicting onset in cases (no imputations)',cex.main = 0.7)

# Secverity Random Forests ----------------------------------------------------
yourSample <- data3_1[,-c(1,2,6,11,12,13,14,15,16,17)] %>%
  group_by(severity) %>%
  sample_n(size = 1334) # currently buggy

yourSample2 <- data3_2[,-c(1,2,6,11,12,13,14,16,17)] %>%
  group_by(severity) %>%
  sample_n(size = 532) # currently buggy

rf_severity_311 <- randomForest(severity ~ ., data = yourSample,  na.action=na.exclude)
rf_severity_312 <- randomForest(severity ~ ., data = yourSample2,  na.action=na.exclude)

varImpPlot(rf_severity_311, main = 'Predicting severity in early onset (with imputations)',cex.main = 0.7)
varImpPlot(rf_severity_312, main = 'Predicting severity in early onset (no imputations)',cex.main = 0.7)


# sev ---------------------------------------------------------------------
yourSample <- data4_1[,-c(1,2,6,11,12,13,14,15,16,17)] %>%
  group_by(severity) %>%
  sample_n(size = 3694) # currently buggy

yourSample2 <- data4_2[,-c(1,2,6,11,12,13,14,16,17)] %>%
  group_by(severity) %>%
  sample_n(size = 1550) # currently buggy

rf_severity_411 <- randomForest(severity ~ ., data = yourSample,  na.action=na.exclude)
rf_severity_412 <- randomForest(severity ~ ., data = yourSample2,  na.action=na.exclude)

varImpPlot(rf_severity_411, main = 'Predicting severity in early onset (with imputations)',cex.main = 0.7)
varImpPlot(rf_severity_412, main = 'Predicting severity in early onset (no imputations)',cex.main = 0.7)
cex.main = 0.7


# Multiclass accuracy -----------------------------------------------------

library(pROC)





par(mfrow=c(2,2))





rf_severity_222 <- randomForest(severity ~ ., data = data2_2[train_ind,],  na.action=na.exclude)
rf_severity_322 <- randomForest(severity ~ ., data = data3_2[train_ind,],  na.action=na.exclude)
rf_severity_422 <- randomForest(severity ~ ., data = data4_2[train_ind,],  na.action=na.exclude)

names(data2_1)
(rf_severity_422$importance) # How important each predictor 


# prediction Asthma --------------------------------------------------------------
(rf_onset_211$importance)
prediction111 <- predict(rf_onset_111, data1_1, type = 'response')
prediction121 <- predict(rf_onset_121, data1_2[-train_ind,], type = 'response')

confusionMatrix(prediction111, data1_1$Doctor.diagnosed.asthma.0.0)
confusionMatrix(prediction121, data1_2$Doctor.diagnosed.asthma.0.0[-train_ind])

# prediction Onset --------------------------------------------------------------
prediction211 <- predict(rf_onset_211, data2_1, type = 'response')
prediction221 <- predict(rf_onset_221, data2_2[-train_ind,], type = 'response')

confusionMatrix(prediction211, data2_1$asthma)
confusionMatrix(prediction221, data2_2$asthma[-train_ind])

# prediction Severity --------------------------------------------------------------

prediction212 <- predict(rf_severity_212, data2_1[-train_ind,], type = 'response')
prediction312 <- predict(rf_severity_312, yourSample, type = 'response')
prediction412 <- predict(rf_severity_412, yourSample, type = 'response')
prediction222 <- predict(rf_severity_222, data2_2[-train_ind,], type = 'response')
prediction322 <- predict(rf_severity_322, data3_2[-train_ind,], type = 'response')
prediction422 <- predict(rf_severity_422, data4_2[-train_ind,], type = 'response')

confusionMatrix(prediction212, data2_1$severity[-train_ind])
confusionMatrix(prediction312, yourSample$severity)
confusionMatrix(prediction412, data4_1$severity[-train_ind])
confusionMatrix(prediction222, data2_2$severity[-train_ind])
confusionMatrix(prediction322, data3_2$severity[-train_ind])
confusionMatrix(prediction422, data4_2$severity[-train_ind])


# tidy --------------------------------------------------------------------
options(scipen = 999)

data1_1 <- na.omit(data1_1)
rfcv((data1_1[, -c(1,45)]), (as.vector(data1_1[,1])))
rf_severity_312

weighted211 <- randomForest(asthma ~ .-Age.asthma.diagnosed.by.doctor.0.0, data = data2_1[train_ind,],  na.action=na.exclude,ntree = 1500, classwt = c(3590, 11901))

# final -------------------------------------------------------------------

earlyc <- read.csv('3_1_with_cluster.csv')
latec <- read.csv('4_1_with_cluster.csv')

yourSample <- earlyc %>%
  group_by(cluster) %>%
  sample_n(size = 2796) 

smp_size <- floor(0.75 * nrow(yourSample))
train_ind <- sample(seq_len(nrow(yourSample)), size = smp_size)

par(mfrow=c(1,2))

earlyran <- randomForest(factor(cluster) ~ ., data = yourSample[train_ind,],  na.action=na.exclude)
varImpPlot(earlyran, main = 'Full Model')
predictione <- predict(earlyran, yourSample[-train_ind,], type = 'response')
confusionMatrix(predictione, factor(yourSample$cluster[-train_ind]))
# --
earlyran2 <- randomForest(factor(cluster) ~ ., data = yourSample[train_ind,-c(3,4,8,13,14,15,16,17,18,19,20)],  na.action=na.exclude)
varImpPlot(earlyran2, main = 'Without Lung Function')
predictione <- predict(earlyran2, yourSample[-train_ind,], type = 'response')
confusionMatrix(predictione, factor(yourSample$cluster[-train_ind]))

dim(varImp(earlyran))
names(earlyc)


# late --------------------------------------------------------------------

yourSample <- latec %>%
  group_by(cluster) %>%
  sample_n(size = 2796) 

smp_size <- floor(0.75 * nrow(latec))
train_ind <- sample(seq_len(nrow(latec)), size = smp_size)
par(mfrow=c(1,2))
earlyran <- randomForest(factor(cluster) ~ ., data = yourSample[train_ind,],  na.action=na.exclude)
varImpPlot(earlyran, main = 'Full Model')
predictione <- predict(earlyran, yourSample[-train_ind,], type = 'response')
confusionMatrix(predictione, factor(yourSample$cluster[-train_ind]))
# --
earlyran2 <- randomForest(factor(cluster) ~ ., data = yourSample[train_ind,-c(3,4,8,13,14,15,16,17,18,19,20)],  na.action=na.exclude)
varImpPlot(earlyran2, main = 'Without Lung Function')
predictione <- predict(earlyran2, yourSample[-train_ind,], type = 'response')
confusionMatrix(predictione, factor(yourSample$cluster[-train_ind]))
