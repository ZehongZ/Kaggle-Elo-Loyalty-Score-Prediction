#Import package
library(data.table)
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(dummies)
library(party)
library(mlbench)
library(caret)
library(rpart)
library(e1071)
library(klaR)
library(mlr)
devtools::install_github("hoxo-m/githubinstall")

#Import dataset
mydx=data.table::fread ('[No transform]full_tb_x.csv',header = T)
dim(mydx)
names(mydx)

mydy=data.table::fread("full_tb_y.csv",header = T)
dim(mydy)
head(mydy)

#Subset dataset
dim(mydx)
dim(mydy)
mydx=mydx[,2:109]
mydy=mydy[,2:3]
names(mydx)
names(mydy)

#Merge dataset
myd=merge(mydx, mydy, on="card_id",all.x = TRUE)
names(myd)
dim(myd)
#Missing values
sum(is.na(myd))

#Variable types
str(myd)

#Split training and testing set
train.size=round(0.66*nrow(myd))
id.train=sample(1:nrow(myd),train.size, replace = FALSE)
train=myd[id.train,]
test=myd[-id.train,]
dim(train)
dim(test)

#PCA
p1=prcomp(myd[,2:109],scale=TRUE,center = TRUE)
summary(p1)#Require 35 components
fviz_eig(p1)
pca_var=get_pca_var(p1)
head(pca_var$coord)#Feature1, feature2, feature3, first_active_month_year_lag_to_reference_date, # of trans_records, # of unique_merchant

#Random Forest
library(party)
cf1<-cforest(train$target~., data=train[,2:109], control=cforest_unbiased(mtry=2, ntree=50))
varimp(cf1)#Variable importance based on mean decrease in accuracy
#Feature 1, feature2, feature3, first_active_month_year_lag_to_reference_date,# of trans_records, # of unique merchant

#Subest dataset based on selected variables
myds=train[,c(2:7)]
dim(myds)

#XGBoost Preparation 
library(xgboost)
setDT(train)
setDT(test)
tr=train[,2:108]
ts=test[,2:108]
trLabel=as.numeric(train$target)
tsLabel=as.numeric(test$target)
new_tr=model.matrix(~.+0, data=tr,with=F)
new_ts=model.matrix(~.+0, data=ts, with=F)
head(new_tr)
head(new_ts)

dtrain<-xgb.DMatrix(data=new_tr, label=trLabel)
dtest<-xgb.DMatrix(data=new_ts, label=tsLabel)

#Parameters setting
params<-list(booster="dart",object="reg:linear",eta=0.3,gamma=0,max_depth=6, min_child_weight=1, subsample=1,colsample_bytree=1,alpha=1)
#Booster: gbtree, gbliner or dart. Regression can use any
#nthread: activates parallel computation. Generally don't change
#eta: Control learning rate. Lower eta to slower computation
#gamma: Controls regularization. 
#max_depth: Controls depth of the tree
#min_child_weight: In regression, it refers to the minimum number of instances required in a child node
#subsample: Controls the number of samples supplied to a tree. Generally [0.5-0.8]
#colsample: Controls the number of feature supplied to a tree. [0.5-0.9]
#lambda: Controls L2 regularization (Ridge regression) on weights. It is used to avoid overfitting
#Alpha: Controls L1 regularization on weights. Also results in feature selectin.



#Using inbuilt xgb.cv for best nround
xgbcv<-xgb.cv(parames=params, data=dtrain, nrounds = 100, nfold=5, showsd=T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F,eval_metric="rmse")
#iteration = 9 is the best
min(xgbcv$test.error.mean)

#Build xgboost model
xgb1<-xgb.train(params = params, data=dtrain, nrounds=9,watchlist = list(val=dtest,train=dtrain),print_every_n = 10,early.stop.rounds=10,maximize = F, eval_metric="rmse")
#val-rmse:3.878798, train-rmse:3.630838
summary(xgb1)

#Prediction
xgbpred<-predict(xgb1,dtest)
xgbpred

#Inforamtion extraction
lbel=getinfo(dtest,"label")
err<-as.numeric(sum(as.integer(xgbpred>0.5) !=tsLabel))/length(tsLabel)
print(paste("test-error=",err))

#Model evaluation
actuals_preds<-data.frame(cbind(actuals=tsLabel,predicteds=xgbpred))
correlation_accuracy<-cor(actuals_preds)
print(correlation_accuracy)
head(actuals_preds)

#View variable importance plot
max<-xgb.importance(feature_names=colnames(new_tr),model=xgb1)
xgb.plot.importance(importance_matrix = max[1:30])

#Train Control
trainControl=trainControl(method="cv",number=10, repeats=3)
seed<-7
metric<-"rmse"

