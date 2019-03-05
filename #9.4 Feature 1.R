#Instaill packages
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(dummies)
library(party)
library(data.table)
library(mlbench)
library(caret)
library(rpart)
library(e1071)

#Input data
historical=read.csv("historical_transaction_cardID_C.csv",header = T)
new_tranc=read.csv("new_merchant_transactions.csv",header = T)
names(historical)
length(unique(historical$card_id))
train=read.csv("train.csv",header = T)
train=train[train$card_id%in%historical$card_id,]
feature=train[,1:3]
dim(feature)
names(feature)
merchants=read.csv("1.csv",header = T)
colnames(merchants)[colnames(merchants)=="X"]<-"merchant_id"

merchants=merchants[merchants$merchant_id%in%historical$merchant_id,]
dim(merchants)
dim(historical)
names(merchants)

#Train Control
trainControl=trainControl(method="repeatedcv",number=10,repeats=3)

#Merge with merchant data
shop=merge(historical,merchants,on="merchant_id",all.x = TRUE)
dim(shop)

#Merge with feature_1
f1shop=merge(shop, feature, on="card_id", all.x = TRUE)
dim(f1shop)
names(f1shop)
f1shop$feature_1=as.factor(f1shop$feature_1)
str(f1shop)

#Merge with feature_2
feature2=train[,c(1:2,4)]
f2shop=merge(shop,feature2,on="card_id",all.x = TRUE)
names(f2shop)
dim(f2shop)

#Merge with feature_3
feature3=train[,c(1:2,5)]
f3shop=merge(shop,feature3,on="card_id",all.x = TRUE)
names(f3shop)
dim(f3shop)

#Merge as a whole data
all=merge(shop,feature,on="card_id",all.x = TRUE)
dim(all)
names(all)

#Subsetting data
str(f1shop)
dim(f1shop)
f1shopnu=f1shop[,c(7:56)]
f11shopnu=is.na(f1shopnu)
dim(f1shopnu)
dim(f11shopnu)
f2shopnu=f2shop[,c(7:56)]
dim(f2shopnu)
f22shopnu=is.na(f2shopnu)
dim(f22shopnu)
f3shopnu=f3shop[,c(7:56)]
dim(f3shopnu)
f33shopnu=is.na(f3shopnu)
dim(f33shopnu)

#Missing value plot
library(VIM)
library(mice)
aggr(f1shopnu, prop=FALSE, numbers=TRUE)


#PCA 
sum(is.na(f1shopnu))
f1shopnu=is.na(f1shopnu)
sum(is.na(f1shopnu))
pf1=prcomp(f1shopnu, center = FALSE, scale. = FALSE)
print(pf1)
summary(pf1)#One component explain 0.9957
pf1_var=get_pca_var(pf1)
head(pf1_var$coord)#Authorized_flag, categoriy_1,installments,category_3,month_lag,purchase_amount

sum(is.na(f2shopnu))
f2shopnu=is.na(f2shopnu)
sum(is.na(f2shopnu))
pf2=prcomp(f2shopnu,center = FALSE, scale. = FALSE)
summary(pf2)#One component explain 0.9957
pf2_var=get_pca_var(pf2)
head(pf2_var$coord)#Authorized_flag, category_1,installments,category_3,month_lag,purchase_amount

sum(is.na(f3shopnu))
f3shopnu=is.na(f3shopnu)
sum(is.na(f3shopnu))
pf3=prcomp(f3shopnu,center=FALSE, scale. = FALSE)
summary(pf3)#One component explain 0.9957
pf3_var=get_pca_var(pf3)
head(pf3_var$coord)#Authorized_flag, category_1, installments, category_3, month_lag, purchase_amount

#Random Forest
f1shopnu=as.data.frame(f1shopnu)
f1shopnu$feature_1=as.factor(f1shopnu$feature_1)
cf1<-cforest(feature_1~., data=f1shopnu)
varimp(cf1)#Random Forest shows these variables are not important 

f2shopnu=as.data.frame(f2shopnu)
cf2<-cforest(feature_2~.,data=f2shopnu)
varimp(cf2)#Random Forest shows these variables are not important 

f3shopnu=as.data.frame(f3shopnu)
cf3<-cforest(feature_3~.,data=f3shopnu)
varimp(cf3)#Random Forest shows these variables are not important 

#KNN
fit=knn3(feature_1~.,data=f1shopnu,k=4)
print(fit)#2:91,3:33,4:136,5:221
f2shopnu$feature_2=as.factor(f2shopnu$feature_2)
fit2=knn3(feature_2~.,data=f2shopnu,k=3)
print(fit2)#1:22,2:421,3:8
f3shopnu$feature_3=as.factor(f3shopnu$feature_3)
fit3=knn3(feature_3~.,data=f3shopnu,k=2)
print(fit3)#0:227,1:254

#SVM
library(kernlab)
library(e1071)
fit.svm=train(feature_1~.,data=f1shopnu,method="svmRadial",trControl=trainControl)
fit2.svm=train(feature_2~.,data=f2shopnu,method="svmRadial",trControl=trainControl)
fit3.svm=train(feature_3~.,data=f3shopnu, method="svmRadial",trControl=trainControl)

