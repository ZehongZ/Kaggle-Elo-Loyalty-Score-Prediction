#Packages
library(caret)
library(klaR)
library(randomForest)
library(rpart)
library(class)
library(data.table)

#Dataset
train=read.csv("train.csv",header = T)
historical=data.table::fread("historical_transactions.csv",header = T)
merchant=data.table::fread("merchants.csv",header = T)
newtransaction=data.table::fread("new_merchant_transactions.csv",header = T)

#Data prepeocessing
move=newtransaction
stay=historical[!(historical$card_id%in%move$card_id),]
move_new=historical[historical$card_id %in% move$card_id,]#27093763
stay_old=historical[historical$card_id%in%stay$card_id,]#2018598

#Feature_1
dim(train)
train=train[which(train$target<=3.25& train$target>=-1.59),]
feature1=train[,1:3]
names(feature1)
feature1His=historical[historical$card_id%in%feature1$card_id]
dim(feature1His)
feature1New=newtransaction[newtransaction$card_id%in%feature1$card_id]
dim(feature1New)
f1His=merge(feature1His,feature1,by="card_id",all.x = TRUE)
f1New=merge(feature1New,feature1, by="card_id",all.x = TRUE)
f1His$feature_1=as.factor(f1His$feature_1)
f1New$feature_1=as.factor(f1New$feature_1)
names(f1His)
names(f1New)

#F1His data type
typeof(f1His$card_id)
typeof(f1His$authorized_flag)#Character
typeof(f1His$city_id)
typeof(f1His$category_1)#Character
typeof(f1His$installments)
typeof(f1His$category_3)#Character
typeof(f1His$merchant_category_id)
typeof(f1His$merchant_id)#Character
typeof(f1His$month_lag)
typeof(f1His$purchase_amount)#Double
typeof(f1His$purchase_date)#Character
typeof(f1His$category_2)#Double
typeof(f1His$state_id)
typeof(f1His$subsector_id)
typeof(f1His$first_active_month)
typeof(f1His$feature_1)

#Subsetting numerical data
f1h=f1His[,c(2,5:6,9:12,15:16)]
f1n=f1New[,c(2,5:6,9:12,15:16)]
head(f1h)
names(f1h)

#Types of variables
typeof(f1h$authorized_flag)#Character
typeof(f1h$installments)
typeof(f1h$category_3)#Character
typeof(f1h$month_lag)
typeof(f1h$purchase_amount)
typeof(f1h$purchase_date)#Character
typeof(f1h$category_2)
typeof(f1h$first_active_month)
typeof(f1h$feature_1)

#Transform data type
library(dummies)
a=f1h$authorized_flag
a=dummy(a)
head(a)

head(a)
#PCA
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
head(f1h$authorized_flag)
sum(is.na(f1His))
f1His=na.omit(f1His)
sum(is.na(f1His))
p1=prcomp(f1His,center=TRUE, scale=TRUE) 
