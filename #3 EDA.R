library(data.table)
historical=data.table::fread("historical_transactions.csv",header = T)
#Missing value plot
library(VIM)
library(mice)
aggr(historical, prop=FALSE, numbers=TRUE)
c1=count(historical, historical$category_1)
dim(c1)
barplot(c1$n, main="Distribution of Category_1",names.arg=c("Y","N"),col=c("blue","Red"))
historical=na.omit(historical)
c2=count(historical, as.factor(historical$category_2))
barplot(c2$n, names.arg = c("1","2","3","4","5"), main="Distribution of Category_2",,col=c("Blue","Yellow","Green","Red","Black"))
c3=count(historical, as.factor(na.omit(historical$category_3)))
c3
barplot(c3$n, names.arg=c("","A","B","C"),main="Distribution of Category_3",col = c("Black","Blue","Green","Yellow"))
levels(as.factor(historical$category_3))
levels(as.factor(historical$installments))
c4=count(historical, as.factor(historical$installments))
barplot(c4$n, names.arg=c("-1","0","1","2","3","4","5","6","7","8","9","10","11","12","999"), main="Distribution of Installments")

plot(historical$purchase_amount)

#Train Table ERD
train=read.csv("train.csv",header = T)
names(train)
dim(train)
library(dplyr)
levels(as.factor(train$feature_1))#1,2,3,4,5
levels(as.factor(train$feature_2))#1,2,3
levels(as.factor(train$feature_3))#0,1

#Distribution
feature1=count(train, train$feature_1)
feature1
barplot(feature1$n,names.arg = c("1","2","3","4","5"),main = "Distributions of Feature_1",col = c("Blue","Green","Red","Yellow","Orange"))

feature2=count(train, train$feature_2)
feature2
barplot(feature2$n, names.arg = c("1","2","3"), main="Distributions of Feature_2",col=c("Blue","Green","Red"))

feature3=count(train, train$feature_3)
feature3
barplot(feature3$n, names.arg = c("0","1"), main="Distributions of Feature_3",col=c("Blue","Red"))

date=count(train, train$first_active_month)
date=date[order(date$n),]
date
mean(date$n)
median(date$n)
barplot(date$n, main="Distributions of First Active Month", col = c("Blue"))
plot(date,main="First Active Month Frequency",ylab="Count",xlab="Month")

library(ggplot2)
library(forecast)
library(plotly)
library(tseries)
library(highfrequency)
library(fBasics)

date=as.data.frame(date)
head(date)
date$Month=date$`First active month`
head(date)
newdate=date[,2:3]
head(newdate)

typeof(newdate$Month)
newdate$Month=as.Date(newdate$Month)
ggplot(newdate, aes(Month, n))+geom_line()+scale_x_date('day')
newdate
write.csv(newdate, "newdate")

newdate=read.csv("newdate.csv", header = T)
head(newdate)
dd=newdate$Month.1
dd
dd=as.Date(dd,format ="$m/$d/%y")
dd

dd=as.Date(dd,format="%m/%d/%Y")
dd
newdate$Month.1=as.Date(newdate$Month.1,format="%m/%d/%y")
head(newdate$Month.1)
head(newdate)
newdate=newdate[,3:4]
head(newdate)

Frequency<-ts(newdate$Month.1,start=c(2012,1),end=c(2018,2),freq=30,"Counts")
plot.ts(Frequency)

boxplot(train$target,main="Boxplot",col="Blue")
