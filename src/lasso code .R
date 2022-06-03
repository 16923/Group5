# clear the space 
rm(list=ls())

# Downloading packages
library(readxl)
library(glmnet)
library(pROC)

library(glmnet); library(caret)
install.packages("InformationValue")
library(InformationValue)
install.packages("ISLR")
library(ISLR)


# 5 k-fold Stability LASSO regression on each dataset - with and without imputation 
# 1. Whole dataset 
# 2. Asthma dataset 
# 3. Early onset 
# 4. Late onset 

# 1. Whole Dataset 
# Importing whole dataset cases and controls- 
## importing the dataset 
# Compiled into one document; the variable names will be similar and the data is also repeated with non-imputed dataset
# with imputations 
d1.12 <- read_excel("data1-1.xlsx")  
# without imputations (complete case)
# d1.1 <- read_excel("/data1-2-2.xlsx")  


d1.11 <- d1.12[,-c(46,76,77)] # removing the unnecessary columns 

train <- sample(1:nrow(d1.12), 0.7 * nrow(d1.12)) # train model 
test <- seq(1, nrow(d1.12))[-train] # test model 

d1.12$asthma <- as.numeric(gsub("No", 0, gsub("Yes", 1, d1.12$asthma))) # re-coding asthma into 0 or 1 
d1.12$Genetic.ethnic.grouping.0.0 <- as.numeric(gsub("NA", 0, gsub("1", 1, d1.12$Genetic.ethnic.grouping.0.0))) # re-coding ethnic grouping into 0 or 1 

d1.12<- na.omit(d1.12) # ommiting any NA values 

Y1 <- d1.1$asthma # setting the outcome 
X1 <- data.matrix(d1.12[,-c(1)]) # removing the Y column and selecting rest of the dataset

model_lasso <- cv.glmnet(X1[train, ], Y1[train], alpha = 1,
                         family = "gaussian") # lasso model 
plot(model_lasso) # plotting lasso model 

# finding optimal value that minimizes test MSE 
model_lasso$lambda.min # minimum lambda metrics 
model_lasso$lambda.1se # lambda 1se value 
bestlam_lasso <- model_lasso$lambda.1se # selects the best lambda value 
length(which(coef(model_lasso, s = bestlam_lasso) !=  # finding coefficeints that are not equal to zero - LASSO 
               0)) - 1

model_lasso_pred <- predict(model_lasso, s = bestlam_lasso,
                            newx = X1[test, ])
mean((model_lasso_pred - Y1[test])^2)

table(coef(model_lasso, s = "lambda.1se")[-1] != 0)
# visualising non-zero components 
betas = coef(model_lasso, s = "lambda.1se")[-1]
names(betas) = rownames(coef(model_lasso, s = "lambda.1se"))[-1]
plot(betas[betas != 0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(betas != 0), cex.axis=0.45, labels = names(betas)[betas !=
                                                                              0], las = 2)
abline(h = 0, lty = 2) 

#find coefficients of best model
best_model <- glmnet(X1, Y1, alpha = 1, lambda = best_lambda)
coef(best_model)
## 5-fold cross validation 
folds <- lapply(split(sample(seq(1, nrow(d1.1), 
                                 by = 2)), rep(1:5, length = length((d1.1$asthma)/2))),
                FUN = function(x) {
                  return(c(x, (x + 1)))
                })
fold_ids <- c(rep(1, length(folds$`1`)), rep(2, length(folds$`2`)),
              rep(3, length(folds$`3`)), rep(4, length(folds$`4`)),
              rep(5, length(folds$`5`)))
ids <- c(folds$`1`, folds$`2`, folds$`3`, folds$`4`,
         folds$`5`)
fold_data <- data.frame(fold_ids, ids)
fold_data <- fold_data[order(fold_data$ids), ]
#X1<-na.omit(X1[-1,])
#Y1<-na.omit(Y1[c(-68)])

cv_logistic_auc <- cv.glmnet(X1, Y1, family = "binomial", type.measure = "auc", foldid = fold_data[-1,]$fold_ids)
plot(cv_logistic_auc)

cv_logistic_auc$lambda.1se

coefficients_logit_1se <- coef(cv_logistic_auc, s = cv_logistic_auc$lambda.1se)
non_zero_index <- coefficients_logit_1se@i
non_zero_coefficients <- coefficients_logit_1se@x
cbind(c("Intercept", colnames(d1.1)[non_zero_index]),
      non_zero_coefficients)
list(coefficients_logit_1se[[1]]) + 
  list(coefficients_logit_1se)
which((coefficients_logit_1se[1]) ) # finding coefficients for 5-fold cross validation 

netFit <- train(x = X1, y = Y1,method = "glmnet", metric = "ROC", tuneGrid=eGrid,trControl = Control)
netFitPerf <- getTrainPerf(netFit) # metric - AUC 


#2. Asthmatics 
# Imputed dataset 
set.seed(544)
d2.1 <- read.csv("/data2-1.csv")  # loading data 
# d2.2 <- read.csv("/data2-2.csv")  # complare-case analysis 
# d2.2 <- d2.2[,-c(32:48,64:65)]

d2.1 <- d2.1[,-c(38:54,71:72)] # selecting relevant columns 

train <- sample(1:nrow(d2.1), 0.7 * nrow(d2.1)) 
test <- seq(1, nrow(d2.1))[-train]

Y1 <- d2.1$asthma
X1 <- data.matrix(d2.1[,-c(1)])

model_lasso <- cv.glmnet(X1[train, ], Y1[train], alpha = 1,
                         family = "gaussian")
plot(model_lasso)
# finding optimal value that minimizes test MSE 
model_lasso$lambda.min
model_lasso$lambda.1se
bestlam_lasso <- model_lasso$lambda.1se
length(which(coef(model_lasso, s = bestlam_lasso) !=
               0)) - 1

model_lasso_pred <- predict(model_lasso, s = bestlam_lasso,
                            newx = X1[test, ])
mean((model_lasso_pred - Y1[test])^2)

table(coef(model_lasso, s = "lambda.1se")[-1] != 0) # coefficients 
# visualising non-zero components 
betas = coef(model_lasso, s = "lambda.1se")[-1]
names(betas) = rownames(coef(model_lasso, s = "lambda.1se"))[-1]
plot(betas[betas != 0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(betas != 0), cex.axis=0.25, labels = names(betas)[betas !=
                                                                              0], las = 2)
abline(h = 0, lty = 2) 
#find coefficients of best model
best_model <- glmnet(X1, Y1, alpha = 1, lambda = best_lambda)
coef(best_model)
## 5-fold cross validation 
folds <- lapply(split(sample(seq(1, nrow(d2.1), 
                                 by = 2)), rep(1:5, length = length((d2.1$asthma)/2))),
                FUN = function(x) {
                  return(c(x, (x + 1)))
                })
fold_ids <- c(rep(1, length(folds$`1`)), rep(2, length(folds$`2`)),
              rep(3, length(folds$`3`)), rep(4, length(folds$`4`)),
              rep(5, length(folds$`5`)))
ids <- c(folds$`1`, folds$`2`, folds$`3`, folds$`4`,
         folds$`5`)
fold_data <- data.frame(fold_ids, ids)
fold_data <- fold_data[order(fold_data$ids), ]

sum(is.na(Y1)) # checking whether this holds NA 
X1 <- na.omit(X1)
Y1 <- Y1[1:15491]
cv_logistic_auc <- cv.glmnet(X1, Y1, family = "binomial", type.measure = "auc", foldid = fold_data[-1,]$fold_ids)
plot(cv_logistic_auc)  # logistic AUC 
netFit <- train(x = X1, y = Y1,method = "glmnet", metric = "ROC", tuneGrid=eGrid,trControl = Control)
netFitPerf <- getTrainPerf(netFit) # metric - AUC 

cv_logistic_auc$lambda.1se

coefficients_logit_1se <- coef(cv_logistic_auc, s = cv_logistic_auc$lambda.1se)
non_zero_index <- coefficients_logit_1se@i
non_zero_coefficients <- coefficients_logit_1se@x
cbind(c("Intercept", colnames(d2.1)[non_zero_index]),
      non_zero_coefficients) # finding non-zero components 

# 3. Early Onset 
d3.1 <- read.csv("/3_1_with_cluster.csv")  
# d3.2 <- read.csv("/TDS/3_2_with_cluster.csv")  completed-case analysis



d3.1 <- d3.1[,-c(1,38,69,70)]
d3.2 <- d3.2[,-c(1,33,64,65)]


train <- sample(1:nrow(d3.1), 0.7 * nrow(d3.1))
test <- seq(1, nrow(d3.1))[-train]



Y1 <- d3.1$cluster
X1 <- data.matrix(d3.1[,-c(69)])

model_lasso <- cv.glmnet(X1[train, ], Y1[train], alpha = 1,
                         family = "gaussian")

plot(model_lasso)



# visualising non-zero components 
betas = coef(model_lasso, s = "lambda.1se")[-1]
names(betas) = rownames(coef(model_lasso, s = "lambda.1se"))[-1]
par(mar = c(13, 6, 1,1 ))
plot(betas[betas != 0], type = "h", col = "navy", lwd = 3,
     xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(betas != 0), cex.axis=0.55, labels = names(betas)[betas !=
                                                                              0], las = 2)
abline(h = 0, lty = 2) 

# -----###########################
#find optimal lambda value that minimizes test MSE
cv_model <- cv.glmnet(X1,Y1,alpha=1)
best_lambda <- cv_model$lambda.min
best_lambda
model_lasso$lambda.min
model_lasso$lambda.1se
bestlam_lasso <- model_lasso$lambda.1se
length(which(coef(model_lasso, s = bestlam_lasso) !=
               0)) - 1

model_lasso_pred <- predict(model_lasso, s = bestlam_lasso,
                            newx = X1[test, ])
mean((model_lasso_pred - Y1[test])^2)

table(coef(model_lasso, s = "lambda.1se")[-1] != 0)
#produce plot of test MSE by lambda value
plot(cv_model) 
# ----------

#find coefficients of best model
best_model <- glmnet(X1, Y1, alpha = 1, lambda = best_lambda)
coef(best_model)
## 5-fold cross validation 
folds <- lapply(split(sample(seq(1, nrow(d3.1), 
                                 by = 2)), rep(1:5, length = length((d3.1$cluster)/2))),
                FUN = function(x) {
                  return(c(x, (x + 1)))
                })
fold_ids <- c(rep(1, length(folds$`1`)), rep(2, length(folds$`2`)),
              rep(3, length(folds$`3`)), rep(4, length(folds$`4`)),
              rep(5, length(folds$`5`)))
ids <- c(folds$`1`, folds$`2`, folds$`3`, folds$`4`,
         folds$`5`)
fold_data <- data.frame(fold_ids, ids)
fold_data <- fold_data[order(fold_data$ids), ]
#X1<-na.omit(X1[-1,])
#Y1<-na.omit(Y1[c(-68)])

cv_logistic_auc <- cv.glmnet(X1, Y1, family = "multinomial", type.measure = "auc", foldid = fold_data[-1,]$fold_ids) # multinomial because 3 clusters pr
plot(cv_logistic_auc)
netFit <- train(x = X1, y = Y1,method = "glmnet", metric = "ROC", tuneGrid=eGrid,trControl = Control)
netFitPerf <- getTrainPerf(netFit) # metric - AUC 

cv_logistic_auc$lambda.1se

coefficients_logit_1se <- coef(cv_logistic_auc, s = cv_logistic_auc$lambda.1se)
non_zero_index <- coefficients_logit_1se@i
non_zero_coefficients <- coefficients_logit_1se@x
cbind(c("Intercept", colnames(d3.1)[non_zero_index]),
      non_zero_coefficients)
list(coefficients_logit_1se[[1]]) + 
  list(coefficients_logit_1se)
which((coefficients_logit_1se[1]))

# 4. Late Onset 

## importing the dataset 

d4.1 <- read.csv("/4_1_with_cluster.csv")  
# d4.2 <- read.csv("/4_2_with_cluster.csv") complete case analysis 

d4.1 <- d4.1[,-c(1,38,69,70)]
d4.2 <- d4.2[,-c(1,48,64,65)]



train <- sample(1:nrow(d4.1), 0.7 * nrow(d4.1))
test <- seq(1, nrow(d4.1))[-train]
Y1 <- d4.1$cluster
X1 <- data.matrix(d4.1[,-c(69)])

model_lasso <- cv.glmnet(X1[train, ], Y1[train], alpha = 1,
                         family = "gaussian")
plot(model_lasso)

model_lasso$lambda.min
model_lasso$lambda.1se
bestlam_lasso <- model_lasso$lambda.1se
length(which(coef(model_lasso, s = bestlam_lasso) !=
               0)) - 1

model_lasso_pred <- predict(model_lasso, s = bestlam_lasso,
                            newx = X1[test, ])


mean((model_lasso_pred - Y1[test])^2)


##########
table(coef(model_lasso, s = "lambda.1se")[-1] != 0)
# visualising non-zero components 
betas = coef(model_lasso, s = "lambda.1se")[-1]
names(betas) = rownames(coef(model_lasso, s = "lambda.1se"))[-1]
par(mar = c(13, 6, 1,1 ))
plot(betas[betas != 0], type = "h", col = "navy", lwd = 3,xaxt = "n", xlab = "", ylab = expression(beta))
axis(side = 1, at = 1:sum(betas != 0), cex.axis=0.45, labels = names(betas)[betas !=
                                                                              0], las = 2)
abline(h = 0, lty = 2) 

# -----###########################
#find optimal lambda value that minimizes test MSE
cv_model <- cv.glmnet(X1,Y1,alpha=1)
best_lambda <- cv_model$lambda.min
best_lambda
#produce plot of test MSE by lambda value
plot(cv_model) 
# ----------

#find coefficients of best model
best_model <- glmnet(X1, Y1, alpha = 1, lambda = best_lambda)
coef(best_model)
## 5-fold cross validation 
folds <- lapply(split(sample(seq(1, nrow(d4.1), 
                                 by = 2)), rep(1:5, length = length((d4.1$cluster)/2))),
                FUN = function(x) {
                  return(c(x, (x + 1)))
                })
fold_ids <- c(rep(1, length(folds$`1`)), rep(2, length(folds$`2`)),
              rep(3, length(folds$`3`)), rep(4, length(folds$`4`)),
              rep(5, length(folds$`5`)))
ids <- c(folds$`1`, folds$`2`, folds$`3`, folds$`4`,
         folds$`5`)
fold_data <- data.frame(fold_ids, ids)
fold_data <- fold_data[order(fold_data$ids), ]
#X1<-na.omit(X1[-1,])
#Y1<-na.omit(Y1[c(-68)])

cv_logistic_auc <- cv.glmnet(X1, Y1, family = "multinomial", type.measure = "auc", foldid = fold_data[-1,]$fold_ids) # multinomial because 3 clusters present
s = 5
par(mar = c(s,s,s,s))
plot(cv_logistic_auc)

cv_logistic_auc$lambda.1se
netFit <- train(x = X1, y = Y1,method = "glmnet", metric = "ROC", tuneGrid=eGrid,trControl = Control)
netFitPerf <- getTrainPerf(netFit) # metric - AUC 
coefficients_logit_1se <- coef(cv_logistic_auc, s = cv_logistic_auc$lambda.1se)
non_zero_index <- coefficients_logit_1se@i
non_zero_coefficients <- coefficients_logit_1se@x
cbind(c("Intercept", colnames(d4.1)[non_zero_index]),
      non_zero_coefficients)

