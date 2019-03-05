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

#Handle Duplicated Merchants ID
duplicated_merchants=merchants[duplicated(merchants$merchant_id)==TRUE]
dim(duplicated_merchants)
print(duplicated_merchants)
mean(duplicated_merchants$avg_sales_lag6)
summary(duplicated_merchants$avg_sales_lag6)
c=duplicated_merchants[!duplicated(duplicated_merchants)]
c=c[!which(c$merchant_id=="M_ID_bd49e37dda" & c$avg_sales_lag6==104.82),]
c=c[!which(c$merchant_id=="M_ID_ef233cff26"&c$avg_sales_lag6==817.53),]
c=c[!which(c$merchant_id=="M_ID_dbbf07ebf0" & c$avg_sales_lag6==75.36),]
c=c[!which(c$merchant_id=="M_ID_30340088f2" & c$avg_sales_lag6==1.00),]
c=c[!which(c$merchant_id=="M_ID_ebbdb42da6" & c$avg_sales_lag6==1.41),]
c=c[!which(c$merchant_id=="M_ID_c2b9ac2ea4" & c$avg_sales_lag6==1.11),]
c=c[!which(c$merchant_id=="M_ID_992a180b15" & c$avg_sales_lag6==5.56),]
c=c[!which(c$merchant_id=="M_ID_d123532c72" & c$avg_sales_lag6==0.99),]
c=c[!which(c$merchant_id=="M_ID_42697d5d44"& c$avg_sales_lag6==0.74),]
c=c[!which(c$merchant_id=="M_ID_6464db3b45" & c$avg_sales_lag6==1.26),]
c=c[!which(c$merchant_id=="M_ID_1802942aaf" & c$avg_sales_lag6==0.85),]
c

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
train.size=round(0.01*nrow(historical))
id.train=sample(1:nrow(historical),train.size, replace=FALSE)
trHistorical=historical[id.train]

test.size=round(0.01*nrow(historical))
id.test=sample(1:nrow(historical),test.size, replace=FALSE)
tsHistorical=historical[id.test]

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

#Remove duplicated columns
myd4=myd3[,c(1,3:5,7:8,13:26,31,33:38,40:41)]
head(myd4)
dim(myd4)
names(myd4)

mydT4=mydT3[,c(1,3:5,7:8,13:26,31,33:38,40:41)]
head(mydT4)

#Convert data frame to data table
myd4=setDT(myd4)
mydT4=setDT(mydT4)
head(myd4)
dim(myd4)
str(myd4)
str(mydT4)
names(myd4)

#Check missing values
table(is.na(myd4))
sapply(myd4, function(x) sum(is.na(x))/length(x))*100

table(is.na(mydT4))
sapply(mydT4, function(x) sum(is.na(x))/length(x))*100

#Convert variables
library(dummies)


#Set all missing value as "Missing
myd4[is.na(myd4)]<-"Missing"
mydT4[is.na(mydT4)]<-"Missing"

#Prepare variables
trLabels<-myd4$target
tsLabels<-mydT4$target

#Prepare dataset
new_tr<-model.matrix(~.+0, data=myd4[,-c("target"),with=F])

#Convert factor to numeric
trLabels<-as.numeric(trLabels)
tsLabels<-as.numeric(tsLabels)

#Prepare matrix
library(xgboost)
dtrain<-xgb.DMatrix(data=new_tr, label=trLabels)
