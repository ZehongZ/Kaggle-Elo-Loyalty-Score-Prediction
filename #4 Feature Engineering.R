#Import dataset
myd1=read.csv("1.csv", header = T)
#Import train
train=read.csv("train.csv", header = T)
#Dimension of the dataset
dim(myd1)
dim(train)
#Variables of the dataset
names(myd1)
#First five rows of the dataset
head(myd1)

#Split myd1
myd1.size=round(0.66*nrow(myd1))
id.myd1=sample(1:nrow(myd1),myd1.size, replace=FALSE)
train.set=myd1[id.myd1,]
train.set=myd1[-id.myd1,]

#PCA on myd1
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
myd1n=myd1[,c(6:45)]
names(myd1n)
p1=prcomp(na.omit(myd1n),center=FALSE, scale=FALSE) 
print(p1)
summary(p1)
str(p1)
p1_var=get_pca_var(p1)
head(p1_var$coord) #PCA shows "avg_sales_lag12", "avg_sales_lag6","avg_sales_lag3", "avg_purchaases_lag3","avg_purchases_lag6","avg_purchases_lag12"

#PCA on train
p2=prcomp(train[,c(3:6)],center=FALSE, scale=FALSE)
print(p2)
summary(p2)
str(p2)
p2_var=get_pca_var(p2)
head(p2_var$coord)#All variables are significant

#Import historical transactions
library(data.table)
library(dplyr)
historical=data.table::fread("historical_transactions.csv",header = T)
head(historical)
dim(historical)
names(historical)

#Import new_merchant 
new_merchant=data.table::fread("new_merchant_transactions.csv",header = T)
head(new_merchant)
dim(new_merchant)
names(new_merchant)

#Import train
train=read.csv("train.csv", header = T)
names(train)
#Resample train dataset
train.size=round(0.66*nrow(train))
id.train=sample(1:nrow(train),train.size, replace=FALSE)
train.set=train[id.train,]
test.set=train[-id.train,]
#Run random forest on Train
library(party)
cf1<-cforest(train.set$target~., data=train.set,controls = cforest_unbiased(mtry=10,ntree=50))
#R crushes

#Run random forest on 1
library(party)
cf1<-cforest(myd1$avg_sales_lag12~., data=myd1, controls = cforest_unbiased(mtry=2, ntree=50))

#Relative Importance on train data
train=read.csv("train.csv", header = T)
library(relaimpo)
lmMod<-lm(target~., data=train)

#Stepwise
all=lm(train.set$avg_sales_lag12~., data=train.set)
