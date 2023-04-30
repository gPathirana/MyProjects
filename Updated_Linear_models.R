install.packages("ggfortify")
library(dplyr)
library(caret)
library(caTools)
library(Metrics)
library(ggfortify)
library(psych)

#Importing data  set
setwd("E:/UOC NOTES/3rd year/sem 2/ST 3082/data analys/archive")
Newdata=read.csv("updated_data_1.csv")

#Importing data  set
setwd("E:/UOC NOTES/3rd year/sem 2/ST 3082/data analys/archive")
Newdata=read.csv("updated_data_1.csv")
Newdata

Newdata$occupation <- (factor(Newdata$occupation))
Newdata$race            <- (factor(Newdata$race           ))
Newdata$sex             <- (factor(Newdata$sex            ))
Newdata$income          <- (factor(Newdata$income         ))
Newdata$marital.statusN <- (factor(Newdata$marital.statusN))
Newdata$educationN      <- (factor(Newdata$educationN     ))
Newdata$workclassN      <- (factor(Newdata$workclassN     ))
Newdata$native.countryN <- (factor(Newdata$native.countryN))

describe(Newdata)
#spliting the data set
set.seed(1234)
dt=sort(sample(nrow(Newdata),nrow(Newdata)*.8))
train=Newdata[dt,]
test=Newdata[-dt,]
############### Multiple linear regression #####################

Multilinear=lm(train$hours.per.week~.,data=train)
summary(Multilinear)

autoplot(Multilinear, which = 5)

#on test set
test.mat <- model.matrix (test$hours.per.week~ ., data = test)
coefi <- coef (Multilinear)
pred.tst<- test.mat[, names (coefi)] %*% coefi


test.mae = caret::MAE(test$hours.per.week,pred.tst)
test.rmse = caret::RMSE(test$hours.per.week,pred.tst)
test.mape = mape(test$hours.per.week,pred.tst)
cat("multiple linear  regression - ","test mape",test.mape,"test RMSE",test.rmse,"test mae",test.mae)

## R-squared value on test data
RSS.tst= sum((test$hours.per.week - pred.tst)^2)
TSS.tst= sum((test$hours.per.week- mean(pred.tst))^2)
R2.tst=1-(RSS.tst/TSS.tst)

# on train set
train.mat <- model.matrix (train$hours.per.week~ ., data = train)
coefi <- coef (Multilinear , id = 15)
pred.tr<- train.mat[, names(coefi)] %*% coefi


train.mae = caret::MAE(train$hours.per.week,pred.tr)
train.rmse = caret::RMSE(train$hours.per.week,pred.tr)
train.mape = mape(train$hours.per.week,pred.tr)
cat("multiple linear  regression - ","train mape",train.mape,"train RMSE",train.rmse,"train mae",train.mae)

## R-squared value on train data
RSS.tr= sum((train$hours.per.week - pred.tr)^2)
TSS.tr= sum((train$hours.per.week- mean(pred.tr))^2)
R2.tr=1-(RSS.tr/TSS.tr)

cat("multiple linear  regression - ","train R2",R2.tr,"test R2",R2.tst)

############### Multiple linear regression with variable selection as base line model #############

##Best subset selection approach
#install.packages("leaps")

library(leaps)

models <- regsubsets (Newdata$hours.per.week~.,data=Newdata)
summary (models)
#performs best sub- regsubsets() set selection by identifying the best model that contains a given number
#of predictors, where best is quantified using RSS
#By default, regsubsets() only reports results up to the best eight-variable model. But the nvmax option can be used
#in order to return as many variables as are desired. Here we fit up to a
#19-variable model.
reg.models <- regsubsets (hours.per.week~., data = train,
                          nvmax = 32)
reg.sum <- summary (reg.models)
reg.sum

data.frame(
  Adj.R2 = which.max(reg.sum$adjr2),
  CP = which.min(reg.sum$cp),
  BIC = which.min(reg.sum$bic)
)
#[26 23 14]

#plot RSS and Adj R2

#par (mfrow = c(2, 2))
#plot (reg.sum$rss , 
     # xlab = " Number of Variables ",
      #ylab = " RSS ", 
      #type = "l")
#plot (reg.sum$adjr2 ,
     # xlab = " Number of Variables ",
    #  ylab = " Adjusted RSq ", 
    #  type = "l")

#maxadjr2=which.max(reg.sum$adjr2)
which.max(reg.sum$adjr2)
#[1] 26

#points(maxadjr2,
       #reg.sum$adjr2[maxadjr2],
       #col = " red ", 
       #cex = 1,
       #pch = 35)

#we can plot the Cp and BIC statistics
#cp
#plot(reg.sum$cp, xlab = " Number of Variables ",
    # ylab = "Cp", type = "l")
#maxcp=which.min(reg.sum$cp)
which.min(reg.sum$cp)
#[1] 23
#points(maxcp, reg.sum$cp[maxcp], col = " red ", cex = 1,
      # pch = 35)


#bic
#plot(reg.sum$bic, xlab = " Number of Variables ",
     #ylab = "bic", type = "l")
#maxbic=which.min(reg.sum$bic)
which.min(reg.sum$bic)
#[1] 14
#points(maxbic, reg.sum$bic[maxbic], col = " red ", cex = 1,
    #   pch = 35)


coef (reg.models,14)

########## Forward and backward selection ###########
regfit.fwd <- regsubsets(Newdata$hours.per.week ~., data = Newdata,
                         nvmax = 35, method = "forward")
fwd.sum=summary (regfit.fwd)

regfit.bwd<-regsubsets(Newdata$hours.per.week ~., data = Newdata ,
                       nvmax = 35, method = "backward")
bwd.sum=summary(regfit.bwd)

data.frame(
  Adj.R2_Fwd = which.max(fwd.sum$adjr2),
  CP_Fwd = which.min(fwd.sum$cp),
  BIC_Fwd = which.min(fwd.sum$bic)
)
#[26 22 16]
coef (regfit.fwd,17)

data.frame(
  Adj.R2_Bwd = which.max(bwd.sum$adjr2),
  CP_Bwd = which.min(bwd.sum$cp),
  BIC_Bwd = which.min(bwd.sum$bic)
)
#[26 22 16]
coef (regfit.bwd,17)

############## using test train set - cross validation ########
#Validation set approach

regfit.best <- regsubsets (hours.per.week ~ ., data = train, nvmax = 32)
summary(regfit.best)
#compute the validati--0on set error for the best model of each model size

test.mat <- model.matrix (hours.per.week~ ., data = test)

#check test mse for each number of combinations

val.errors <- rep (NA, 32)
for (i in 1:32) {
  coefi <- coef (regfit.best , id = i)
  pred <- test.mat[, names (coefi)] %*% coefi
  val.errors[i] <- mean ((test$hours.per.week - pred)^2)
}
val.errors

minvaler=which.min(val.errors) #27
coef(regfit.best , minvaler)

#checking with full data set 
regfit.best <- regsubsets (Newdata$hours.per.week ~ ., data = Newdata,
                           nvmax =32)
coef (regfit.best , 14)

#get prediction on best subset selection with 13 variables ###

library(dplyr)
library(caret)
library(caTools)
library(Metrics)
library(leaps)
#model
regfit.best.tr <- regsubsets (train$hours.per.week ~ ., data = train, nvmax = 35, method="forward")
#on test set
test.mat <- model.matrix (test$hours.per.week~ ., data = test)
coefi <- coef (regfit.best.tr , id = 14)
pred.tst<- test.mat[, names (coefi)] %*% coefi


test.mae = caret::MAE(test$hours.per.week,pred.tst)
test.rmse = caret::RMSE(test$hours.per.week,pred.tst)
test.mape = mape(test$hours.per.week,pred.tst)
cat("multiple linear  regression - ","test mape",test.mape,"test RMSE",test.rmse,"test mae",test.mae)

## R-squared value on test data
RSS.tst= sum((test$hours.per.week - pred.tst)^2)
TSS.tst= sum((test$hours.per.week- mean(pred.tst))^2)
R2.tst=1-(RSS.tst/TSS.tst)

# on train set
train.mat <- model.matrix (train$hours.per.week~ ., data = train)
coefi <- coef (regfit.best.tr , id = 15)
pred.tr<- train.mat[, names(coefi)] %*% coefi


train.mae = caret::MAE(train$hours.per.week,pred.tr)
train.rmse = caret::RMSE(train$hours.per.week,pred.tr)
train.mape = mape(train$hours.per.week,pred.tr)
cat("multiple linear  regression - ","train mape",train.mape,"train RMSE",train.rmse,"train mae",train.mae)

## R-squared value on train data
RSS.tr= sum((train$hours.per.week - pred.tr)^2)
TSS.tr= sum((train$hours.per.week- mean(pred.tr))^2)
R2.tr=1-(RSS.tr/TSS.tr)

cat("multiple linear  regression - ","train R2",R2.tr,"test R2",R2.tst)





############# Ridge regression  ###########################

library(glmnet)

xtrain=model.matrix(hours.per.week~.,train)[,-1]
ytrain= train$hours.per.week
dim(xtrain)

xtest= model.matrix(hours.per.week~.,test)[,-1]
ytest=test$hours.per.week
#grid=10^seq(10,-2,length=100)

##finding best lambda
set.seed(1)
fit=cv.glmnet(xtrain,ytrain,alpha=0,nfolds=10)
plot(fit)
best_lambda=fit$lambda.min
best_lambda 

#ridge.mod = glmnet (xtrain, ytrain, alpha = 0,lambda = grid, thresh = 1e-12)  ##without best lambda,giving the all grid values
#model
ridge.mod = glmnet (xtrain, ytrain, alpha = 0,lambda = best_lambda)

#prediction on test set
pred.tst = predict(ridge.mod , s = best_lambda, newx = xtest)

mse=mean((ytest-pred.tst)^2)

test.mae = caret::MAE(test$hours.per.week,pred.tst)
test.rmse = caret::RMSE(test$hours.per.week,pred.tst)
test.mape = mape(test$hours.per.week,pred.tst)
cat("Ridge regression - "," test mape",test.mape,"test RMSE",test.rmse,"test mae",test.mae)

## R-squared value on test data
RSS.tst= sum((test$hours.per.week - pred.tst)^2)
TSS.tst= sum((test$hours.per.week- mean(pred.tst))^2)
R2.tst=1-(RSS.tst/TSS.tst)


# on train set
pred.tr = predict(ridge.mod , s = best_lambda, newx = xtrain)

train.mae = caret::MAE(train$hours.per.week,pred.tr)
train.rmse = caret::RMSE(train$hours.per.week,pred.tr)
train.mape = mape(train$hours.per.week,pred.tr)
cat("Ridge regression - ","train mape",train.mape,"train RMSE",train.rmse,"train mae",train.mae)

## R-squared value on train data
RSS.tr= sum((train$hours.per.week - pred.tr)^2)
TSS.tr= sum((train$hours.per.week- mean(pred.tr))^2)
R2.tr=1-(RSS.tr/TSS.tr)

cat("Ridge regression - ", "train R2",R2.tr,"test R2",R2.tst)

out=glmnet(xtrain,ytrain,alpha=0)
predict(out,type="coefficients",s=best_lambda)[1:32,]

############## lasso regression ###################

set.seed(1)
fit1=cv.glmnet(xtrain,ytrain,alpha=1,nfolds=5)
plot(fit1)
best_lambda1=fit1$lambda.min
best_lambda1

#model
lasso.mod=glmnet (xtrain, ytrain, alpha = 1,lambda =best_lambda1 )


#prediction on test data
pred.tst=predict(lasso.mod , s = best_lambda1 ,newx = xtest)

test.mae = caret::MAE(test$hours.per.week,pred.tst)
test.rmse = caret::RMSE(test$hours.per.week,pred.tst)
test.mape = mape(test$hours.per.week,pred.tst)
cat("Lasso regression - "," test mape",test.mape,"test RMSE",test.rmse,"test mae",test.mae)

## R-squared value on test data
RSS.tst= sum((test$hours.per.week - pred.tst)^2)
TSS.tst= sum((test$hours.per.week- mean(pred.tst))^2)
R2.tst=1-(RSS.tst/TSS.tst)


#on train set

pred.tr=predict(lasso.mod , s = best_lambda1 ,newx = xtrain)

train.mae = caret::MAE(train$hours.per.week,pred.tr)
train.rmse = caret::RMSE(train$hours.per.week,pred.tr)
train.mape = mape(train$hours.per.week,pred.tr)
cat("Lasso regression - ","train mape",train.mape,"train RMSE",train.rmse,"train mae",train.mae)

## R-squared value on train data
RSS.tr= sum((train$hours.per.week - pred.tr)^2)
TSS.tr= sum((train$hours.per.week- mean(pred.tr))^2)
R2.tr=1-(RSS.tr/TSS.tr)

cat("Lasso regression - ", "train R2",R2.tr,"test R2",R2.tst)



out=glmnet(xtrain,ytrain,alpha=1)
lasso.coef=predict(out,type="coefficients",s=best_lambda)[1:32,]
lasso.coef[lasso.coef!=0]

############## elastic net regression ################

set.seed(1)
fit_elastic=cv.glmnet(xtrain,ytrain,alpha=0.5,nfolds=5)
plot(fit_elastic)
best_lambda2=fit_elastic$lambda.min
best_lambda2

#model
enet.mod=glmnet (xtrain, ytrain, alpha = 1,lambda =best_lambda2 )


#prediction on test data
pred.tst=predict(enet.mod , s = best_lambda2 ,newx = xtest)

test.mae = caret::MAE(test$hours.per.week,pred.tst)
test.rmse = caret::RMSE(test$hours.per.week,pred.tst)
test.mape = mape(test$hours.per.week,pred.tst)
cat("EN regression - "," test mape",test.mape,"test RMSE",test.rmse,"test mae",test.mae)


## R-squared value on test data
RSS.tst= sum((test$hours.per.week - pred.tst)^2)
TSS.tst= sum((test$hours.per.week- mean(pred.tst))^2)
R2.tst=1-(RSS.tst/TSS.tst)



#on train set

pred.tr=predict(enet.mod , s = best_lambda2 ,newx = xtrain)

train.mae = caret::MAE(train$hours.per.week,pred.tr)
train.rmse = caret::RMSE(train$hours.per.week,pred.tr)
train.mape = mape(train$hours.per.week,pred.tr)
cat("EN regression - ","train mape",train.mape,"train RMSE",train.rmse,"train mae",train.mae)

## R-squared value on train data
RSS.tr= sum((train$hours.per.week - pred.tr)^2)
TSS.tr= sum((train$hours.per.week- mean(pred.tr))^2)
R2.tr=1-(RSS.tr/TSS.tr)

cat("EN regression - ", "train R2",R2.tr,"test R2",R2.tst)

out=glmnet(xtrain,ytrain,alpha=0.5)
enet.coef=predict(out,type="coefficients",s=best_lambda)[1:32,]
enet.coef[enet.coef!=0]
#################################################################################





















