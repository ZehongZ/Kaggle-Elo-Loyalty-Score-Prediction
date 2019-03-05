#Package
library(data.table)
library(highfrequency)

#Input data
train=read.csv("train.csv",header = T)
historical=data.table::fread("historical_transactions.csv",header = T)
mn=read.csv("max_record_norm_tb.csv",header = T)
merchant=read.csv("new_merchant_transactions.csv",header = T)
summary(train$target)
#High and low loyal 
dim(mn)
unique(mn$card_id)
c1=train[train$card_id=="C_ID_0cd2ce025c",]#Firts active month 2017-01
c1
c2=train[train$card_id=="C_ID_5ccc07beb9",]#First active month 2016-11
c2
c1h=historical[historical$card_id%in%c1$card_id,]
c2h=historical[historical$card_id%in%c2$card_id,]
dim(c1h)
dim(c2h)
head(c1h)
head(c2h)
length(unique(c1h$purchase_date))#High score, more purchases dates
length(unique(c2h$purchase_date))#Low score, fewer purchases dates

c1h[order(c1h$purchase_date)]#First purchases date 2017-01-07
c2h[order(c2h$purchase_date)]#First purchases date 2017-01-03

sum(c1h$purchase_amount)#-2088.013
sum(c2h$purchase_amount)#-1242.352

c1h[order(c1h$month_lag)]#Higher score, smaller lag
c2h[order(c2h$month_lag)]#Lower score, bigger lag
summary(c1h$month_lag)
summary(c2h$month_lag)

#Time series
var=c("purchase_date","purchase_amount")
names(c1h)
c1t=c1h[,10:11]
c2t=c2h[,10:11]
c1t
c2t
c1t$purchase_date=as.Date(c1t$purchase_date)
c2t$purchase_date=as.Date(c2t$purchase_date)
c1t<-c1t[,c(2,1)]
c2t<-c2t[,c(2,1)]
c1t$purchase_date=as.Date(c1t$purchase_date)
c2t$purchase_date=as.Date(c2t$purchase_date)
c1t
c2t
length(unique(c1t$purchase_date))
c1=aggregate(c1t$purchase_amount,by=list(c1t$purchase_date),sum)
dim(c1)
c2=aggregate(c2t$purchase_amount,by=list(c2t$purchase_date),sum)
c1
c2
names(c1)
c1[order("Group.1")]#2017-01-07,2017-11-30
c2[order("Group.1")]#2017-01-07,2018-02-28

a=ts(c1$Group.1,start = c(2017,1),end=c(2017,11),freq=1)
ts.plot(a)
b=ts(c2$Group.1,start=c(2017,1),end=c(2017,11),freq=1)
ts.plot(b)

par(mfrow=c(2,1))
plot(c1,main="high",type="b")#Higher score, bigger change
plot(c2,main="low",type="b")#Lower score, smaller change
par()

#Density plot
par(mfrow=c(2,1))
a=density(c1$x)
plot(a)
b=density(c2$x)
plot(b)
par()

c1$card_id%in%merchant$card_id#Move to new mrechant
c2$card_id%in%merchant$card_id#Move to new merchant

