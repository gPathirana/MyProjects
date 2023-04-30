library(dplyr)
library(caret)
library(caTools)
library(Metrics)
library(randomForest)


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
str(Newdata)
num_categories <- sapply(Newdata, function(x) if(is.factor(x)) nlevels(x) else 0)
sum(num_categories)
#spliting the data set
set.seed(1234)
dt=sort(sample(nrow(Newdata),nrow(Newdata)*.8))
train=Newdata[dt,]
test=Newdata[-dt,]


# Fitting Random Forest to the train dataset

set.seed(120)  # Setting seed

xtrain=model.matrix(hours.per.week~.,train)[,-1]
ytrain= train$hours.per.week
dim(xtrain)

xtest= model.matrix(hours.per.week~.,test)[,-1]
ytest=test$hours.per.week

set.seed(100)
rf_model=randomForest(hours.per.week~.,data=train, ntree=800, mtry=8, importance=T,sampsize = 500)
print(rf_model)

#prediction on test1 set
pred_rf_test=predict(rf_model,test)
test.mae.rf = caret::MAE(test$hours.per.week,pred_rf_test)
test.rmse.rf = caret::RMSE(test$hours.per.week,pred_rf_test)
test.mape.rf = mape(test$hours.per.week,pred_rf_test)
cat("test mape",test.mape.rf,"test RMSE",test.rmse.rf,"test mae",test.mae.rf)


#prediction on train1 set
pred_rf_train=predict(rf_model,train)
train.mae.rf = caret::MAE(train$hours.per.week,pred_rf_train)
train.rmse.rf = caret::RMSE(train$hours.per.week,pred_rf_train)
train.mape.rf = mape(train$hours.per.week,pred_rf_train)
cat("train1 mape",train.mape.rf,"train1 RMSE",train.rmse.rf,"train1 mae",train.mae.rf)

## R-squared value on train1 data
RSS.rf.tr= sum((train$hours.per.week - pred_rf_train)^2)
TSS.rf.tr= sum((train$hours.per.week- mean(train$hours.per.week))^2)
R2.rf.tr=1-(RSS.rf.tr/TSS.rf.tr)


## R-squared value on test1 data
RSS.rf.tst= sum((test$hours.per.week - pred_rf_test)^2)
TSS.rf.tst= sum((test$hours.per.week- mean(test$hours.per.week))^2)
R2.rf.tst=1-(RSS.rf.tst/TSS.rf.tst)

cat("train1 R2",R2.rf.tr,"test1 R2",R2.rf.tst)

attributes(rf_model) # get all attributes in randomforest model
rf_model$mse # give mse of each tree
rf_model$rsq # give R_squred value for each tree

#out of bag Error rates for random forest model(mse and rmse)
plot(rf_model)

print(rf_model)

# #Tune Mtry
# tuneRF(train1[,-1],train1[,1],
#        stepFactor = 2, #mekatat thwa values dala balnna one poddak.
#        plot = TRUE,
#        ntreeTry = 200,
#        improve = 0.05)

#number of nodes for the tree
hist(treesize(rf_model),
     main = "No of nodes for the tree",
     col ="light blue" )

## Variable Importance
varImpPlot(rf_model)
varImpPlot(rf_model,
           sort = T,
           n.var = 8,
           main = "Top 8 - Variable Importance")

importance(rf_model) #for quantitative values 
varUsed(rf_model)#How many times each var used in decision trees.

#################### XG boost ##############################
#install.packages("xgboost")
library(xgboost)

grid_tune = expand.grid(nrounds = c(200,300,500),#number of trees
                        max_depth = c(4,6,8),
                        eta =0.05, #c(0.025,0.05,0.1,0.3),#learning rates 0.3
                        gamma = 0.05 , #C(0,0.05,0.1,0.5,0.7,0.9,1.0),#0,#pruning should be tuned, i.e. 
                        colsample_bytree = 0.9, # c(0.4,0.6,0.8,1.0), #subsample-ratio of columns for tree , 1
                        min_child_weight =1,#c(1,2,3), #the larger, the more conservative models #is: can be used as stop , 1
                        subsample =0.9)#c(0.5,0.75,1.0)) #c(0.5,0.75,1.0) # used for prevevent overfitting by sampling x% ,1

train1_control = trainControl(method = "cv",
                              number = 5,#number of folds
                              verboseIter = TRUE,
                              allowParallel = TRUE)

xgb_tune2 = train(x=xtrain,
                  y=ytrain,
                  trControl = train1_control,
                  tuneGrid = grid_tune,
                  method = "xgbTree",
                  verbose = TRUE)

xgb_tune2

# #RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 200, max_depth = 6, eta = 0.1, gamma = 0.1, colsample_bytree =
#   0.8, min_child_weight = 1 and subsample = 0.8.

#train1 function included in caret library


#prediction on  test1
xgb.pred.tst = predict(xgb_tune2,xtest)

test1.mse.xg = mean((test$hours.per.week-xgb.pred.tst)^2)
test1.mae.xg = caret::MAE(test$hours.per.week,xgb.pred.tst)
test1.rmse.xg = caret::RMSE(test$hours.per.week,xgb.pred.tst)
test1.mape.xg = mape(test$hours.per.week,xgb.pred.tst)
cat("test1 mape",test1.mape.xg,"test1 RMSE",test1.rmse.xg,"test1 mae",test1.mae.xg)



#prediction on train1 
xgb.pred.tr = predict(xgb_tune2,xtrain)

train1.mse.xg = mean((train$hours.per.week-xgb.pred.tr)^2)
train1.mae.xg = caret::MAE(train$hours.per.week,xgb.pred.tr)
train1.rmse.xg = caret::RMSE(train$hours.per.week,xgb.pred.tr)
train1.mape.xg = mape(train$hours.per.week,xgb.pred.tr)
cat("train1 mape",train1.mape.xg,"train1 RMSE",train1.rmse.xg,"train1 mae",train1.mae.xg)


## R-squared value on train data
RSS.xg.tr= sum((train$hours.per.week-xgb.pred.tr)^2)
TSS.xg.tr= sum((train$hours.per.week- mean(train$hours.per.week))^2)
R2.xg.tr=1-(RSS.xg.tr/TSS.xg.tr)

## R-squared value on test data
RSS.xg.tst= sum((test$hours.per.week - xgb.pred.tst)^2)
TSS.xg.tst= sum((test$hours.per.week- mean(test$hours.per.week))^2)
R2.xg.tst=1-(RSS.xg.tst/TSS.xg.tst)

cat("train R2",R2.xg.tr,"test R2",R2.xg.tst)

# Train the XGBoost model
xgb_model <- xgboost(data = xtrain, label = ytrain, nrounds = 400, max_depth = 1, eta = 0.4, gamma = 0.3, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8)

# Calculate feature importance
importance_matrix <- xgb.importance(feature_names = colnames(xtrain), model = xgb_model)

# Plot feature importance
xgb.plot.importance(importance_matrix)

#prediction on  test1
xgb.pred.tst = predict(xgb_model,xtest)

test1.mse.xg = mean((test$hours.per.week-xgb.pred.tst)^2)
test1.mae.xg = caret::MAE(test$hours.per.week,xgb.pred.tst)
test1.rmse.xg = caret::RMSE(test$hours.per.week,xgb.pred.tst)
test1.mape.xg = mape(test$hours.per.week,xgb.pred.tst)
cat("test1 mape",test1.mape.xg,"test1 RMSE",test1.rmse.xg,"test1 mae",test1.mae.xg)



#prediction on train1 
xgb.pred.tr = predict(xgb_model,xtrain)

train1.mse.xg = mean((train$hours.per.week-xgb.pred.tr)^2)
train1.mae.xg = caret::MAE(train$hours.per.week,xgb.pred.tr)
train1.rmse.xg = caret::RMSE(train$hours.per.week,xgb.pred.tr)
train1.mape.xg = mape(train$hours.per.week,xgb.pred.tr)
cat("train1 mape",train1.mape.xg,"train1 RMSE",train1.rmse.xg,"train1 mae",train1.mae.xg)


## R-squared value on train data
RSS.xg.tr= sum((train$hours.per.week-xgb.pred.tr)^2)
TSS.xg.tr= sum((train$hours.per.week- mean(train$hours.per.week))^2)
R2.xg.tr=1-(RSS.xg.tr/TSS.xg.tr)

## R-squared value on test data
RSS.xg.tst= sum((test$hours.per.week - xgb.pred.tst)^2)
TSS.xg.tst= sum((test$hours.per.week- mean(test$hours.per.week))^2)
R2.xg.tst=1-(RSS.xg.tst/TSS.xg.tst)

cat("train R2",R2.xg.tr,"test R2",R2.xg.tst)














