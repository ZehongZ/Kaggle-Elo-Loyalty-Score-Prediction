#Impart dataset
library(data.table)
train=read.csv("train.csv", header = T)
historical=data.table::fread("historical_transactions.csv",header = T)
merchant=data.table::fread("merchants.csv",header = T)
newtransaction=data.table::fread("new_merchant_transactions.csv",header = T)

#Dimension of data
dim(train)
dim(historical)
dim(merchant)
dim(newtransaction)
names(historical)
names(merchant)

#Data prepeocessing
move=newtransaction
stay=historical[!(historical$card_id%in%move$card_id),]
#Verify data set
f=stay$card_id%in%move$card_id
"TRUE" %in% f
TRUE %in% f
move_new=historical[historical$card_id %in% move$card_id,]#27093763
stay_old=historical[historical$card_id%in%stay$card_id,]#2018598
stay_old2=historical[!(historical$card_id%in%move$card_id),]#2018598,for verification
dim(move_new)
dim(stay_old)
dim(historical)#29112361=nl+l

length(unique(move_new$card_id))#290001 card holders move to new merchant
length(unique(stay_old$card_id))#35539 card holders stay in original merchant
length(unique(newtransaction$card_id))#290001 fro veryfication

#Seperate two groups of cucstomers
dim(train)
mv2new=train[train$card_id%in%move_new$card_id,]
xmv2new=train[train$card_id%in%stay_old$card_id,]
dim(xmv2new)#21931 card holders stay in original merchant
dim(mv2new)#179986 card holders move to new merchant
179986+21931
dim(train)#201917=xmv2new+mv2new

#Verify two groups of customers
xmv2new$card_id%in%newtransaction$card_id #All FALSE
mv2new$card_id%in%newtransaction$card_id #All NEW

#Loyalty scores comparison
summary(mv2new$target)
summary(xmv2new$target)

#Feature comparison
library(dplyr)
names(mv2new)
names(xmv2new)
levels(as.factor(mv2new$feature_1))
par(mfrow=c(3,2))
mv_f1=count(mv2new,mv2new$feature_1)
mv_f1=as.data.frame(mv_f1)
barplot(mv_f1$n,names.arg = c("1","2","3","4","5"),main="mv2new feature1")
xmv_f1=count(xmv2new, xmv2new$feature_1)
xmv_f1=as.data.frame(xmv_f1)
barplot(xmv_f1$n,names.arg = c("1","2","3","4","5"),main="xmv2new feature1")
levels(as.factor(mv2new$feature_2))
mv_f2=count(mv2new,mv2new$feature_2)
mv_f2=as.data.frame(mv_f2)
barplot(mv_f2$n,names.arg = c("1","2","3"), main="mv2new feature2")
xmv_f2=count(xmv2new,xmv2new$feature_2)
xmv_f2=as.data.frame(xmv_f2)
barplot(xmv_f2$n,names.arg = c("1","2","3"), main="xmv2new feature2")
levels(as.factor(mv2new$feature_3))
mv_f3=count(mv2new,mv2new$feature_3)
mv_f3=as.data.frame(mv_f3)
barplot(mv_f3$n,names.arg = c("0","1"),main="mv2new")
xmv_f3=count(xmv2new,xmv2new$feature_3)
xmv_f3=as.data.frame(xmv_f3)
barplot(xmv_f3$n,names.arg = c("0","1"),main="xmv2new feature3")
par()



