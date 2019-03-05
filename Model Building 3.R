#Import packages
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

#Import table
historical=data.table::fread("historical_transactions.csv",header = T)
merchants=data.table::fread("merchants.csv",header = T)
new_merchant_transactions=data.table::fread("new_merchant_transactions.csv",header = T)
train=data.table::fread("train.csv")

#Dimension
dim(historical)
dim(merchants)
dim(new_merchant_transactions)
dim(train)

#Names
names(historical)
names(merchants)
names(new_merchant_transactions)
names(train)

#Subset historical transaction dataset
historical=historical[,c(1:2,4:6,8:10)]
names(historical)

#Subset merchant transaction dataset
merchants=merchants[,c(1,5,7:8,11,13)]
names(merchants)

#Remove duplicated Merchants ID
merchants=merchants[!which(merchants$merchant_id=="M_ID_bd49e37dda" & merchants$avg_sales_lag6==104.82),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_ef233cff26" & merchants$avg_sales_lag6==817.53),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_dbbf07ebf0" & merchants$avg_sales_lag6==75.36),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_30340088f2" & merchants$avg_sales_lag6==1.00),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_ebbdb42da6" & merchants$avg_sales_lag6==1.41),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_c2b9ac2ea4" & merchants$avg_sales_lag6==1.11),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_992a180b15" & merchants$avg_sales_lag6==5.56),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_d123532c72" & merchants$avg_sales_lag6==0.99),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_42697d5d44" & merchants$avg_sales_lag6==0.74),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_6464db3b45" & merchants$avg_sales_lag6==1.26),]
merchants=merchants[!which(merchants$merchant_id=="M_ID_1802942aaf" & merchants$avg_sales_lag6==0.85),]
merchants=merchants[!duplicated(merchants)]
merchants[duplicated(merchants)]


#Subset new_merchant_transactions dataset
new_merchant_transactions=new_merchant_transactions[,c(1,2,4,5:6,9:10)]
names(new_merchant_transactions)

#Remove duplicated new_merchant_transactions 
new_merchant_transactions=new_merchant_transactions[!duplicated(new_merchant_transactions)]
new_merchant_transactions[duplicated(new_merchant_transactions)]
names(new_merchant_transactions)

#Subset train dataset
train=train[,c(1,2,3,6)]
names(train)
str(train)

#Spliting Training and Testing
train.size=round(0.00023*nrow(historical))
id.train=sample(1:nrow(historical),train.size, replace=FALSE)
trHistorical=historical[id.train]
dim(trHistorical)
test.size=round(0.00023*nrow(historical))
id.test=sample(1:nrow(historical),test.size, replace=FALSE)
tsHistorical=historical[id.test]
dim(tsHistorical)

#Join historical & merchants
library(sqldf)
myd=sqldf("select*from trHistorical left outer join merchants on trHistorical.merchant_id = merchants.merchant_id")
head(myd)
dim(myd)

mydT=sqldf("select*from tsHistorical left outer join merchants on tsHistorical.merchant_id = merchants.merchant_id")
head(mydT)
dim(mydT)

#Handling missing values (2%)
sum(is.na(myd))
8242/303099

#Join historical & merchants & new_merchants_transactions
myd2=sqldf("select*from myd left outer join new_merchant_transactions on myd.card_id = new_merchant_transactions.card_id")
head(myd2)
dim(myd2)

mydT2=sqldf("select*from mydT left outer join new_merchant_transactions on mydT.card_id = new_merchant_transactions.card_id")
head(mydT2)
dim(mydT2)

#Join historical & merchants & new_merchants_transactions & train
myd3=sqldf("select*from myd2 left outer join train on myd2.card_id = train.card_id")
head(myd3)
names(myd3)

mydT3=sqldf("select*from mydT2 left outer join train on mydT2.card_id = train.card_id")
head(mydT3)
dim(mydT3)
str(myd3)

#Remove duplicated columns
myd4=myd3[,c(1,3:5,7:8,10,12:15,17:22,24:25)]
head(myd4)
dim(myd4)
names(myd4)

mydT4=mydT3[,c(1,3:5,7:8,10,12:15,17:22,24:25)]
head(mydT4)


  
#Transfer data type
library(fastDummies)
myd5=fastDummies::dummy_cols(myd4,select_columns = c("authorized_flag","category_1","category_3","most_recent_sales_range","authorized_flag..15","category_1..17","category_3..19"))
mydT5=fastDummies::dummy_cols(mydT4,select_columns = c("authorized_flag","category_1","category_3","most_recent_sales_range","authorized_flag..15","category_1..17","category_3..19"))

#Resubset dataset
myd6=myd5[,c(3,5,6,7,9,10,13,15:43)]
mydT6=mydT5[,c(3,5,6,7,9,10,13,15:43)]

#Date format transformation
myd6$first_active_month=as.Date.character(myd6$first_active_month,"%Y-%M")
mydT6$first_active_month=as.Date.character(mydT6$first_active_month,"%Y-%M")
myd6$first_active_month=as.POSIXct(myd6$first_active_month, format="%Y-%M")
mydT6$first_active_month=as.POSIXct(mydT6$first_active_month, format="%Y-%M")
myd6$first_active_month=as.numeric(myd6$first_active_month)
mydT6$first_active_month=as.numeric(mydT6$first_active_month)
str(myd6)

#Convert data frame to data table
setDT(myd6)
setDT(mydT6)

#Check missing values
table(is.na(myd6))
table(is.na(mydT6))

#Set all missing value as "Missing"
myd6[is.na(myd6)]<-'Missing'
mydT6[is.na(mydT6)]<-'Missing'

#Create labels
TrLabels<-myd6$target
TsLabels<-mydT6$target

#Create matrix
new_tr<-model.matrix(~.+0, data=myd6[,-c('target'),with=F])
new_ts<-model.matrix(~.+0, data=mydT6[,-c('target'),with=F])

#Convert factor to numeric
TrLabels<-as.numeric(TrLabels)
TsLabels<-as.numeric(TsLabels)

#Prepare xgBoost
library(xgboost)
dtrain<-xgb.DMatrix(data=new_tr, label=TrLabels)
dtest<-xgb.DMatrix(data=new_ts, label=TsLabels)

#Parameters setting
params<-list(booster="dart",object="multi:softmax",eta=0.3,gamma=0,max_depth=6, min_child_weight=1, subsample=1,colsample_bytree=1,alpha=1)

#Using inbuilt xgb.cv for best nround
xgbcv<-xgb.cv(parames=params, data=dtrain, nrounds = 100, nfold=5, showsd=T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F,eval_metric="rmse")
#iteration = 8 is the best
min(xgbcv$test.error.mean)

#Build xgboost model
xgb1<-xgb.train(params = params, data=dtrain, nrounds=8,watchlist = list(val=dtest,train=dtrain),print_every_n = 10,early.stop.rounds=10,maximize = F, eval_metric="rmse")
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