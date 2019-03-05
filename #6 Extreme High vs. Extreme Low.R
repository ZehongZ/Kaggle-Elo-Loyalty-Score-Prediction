#Instaill library
library(data.table)
#Create negative function
neg=function(x)-x
#Input data
train=read.csv("train.csv",header=T)
historical=data.table::fread("historical_transactions.csv",header = T)
low=read.csv("outlier_lst_L.csv",header=T)
head(low)
#Extreme High vs Extreme Low
eh=train[which(train$target>3.25),]
c<-neg(1.59)
el=train[train$card_id%in%low$card_id,]
el
#Difference analysis
summary(eh$target)
summary(el$target)
levels(as.factor(el$feature_1))
levels(as.factor(el$feature_2))
levels(as.factor(el$feature_3))
el_f1=count(el,el$feature_1)
el_f1=as.data.frame(el_f1)
el_f2=count(el,el$feature_2)
el_f2=as.data.frame(el_f2)
el_f3=count(el,el$feature_3)
el_f3=as.data.frame(el_f3)
eh_f1=count(eh,eh$feature_1)
eh_f1=as.data.frame(eh_f1)
eh_f2=count(eh,eh$feature_2)
eh_f2=as.data.frame(eh_f2)
eh_f3=count(eh,eh$feature_3)
eh_f3=as.data.frame(eh_f3)
par(mfrow=c(3,2))
barplot(el_f1$n,names.arg = c("1","2","3","4","5"))
barplot(eh_f1$n,names.arg = c("1","2","3","4","5"))
barplot(el_f2$n,names.arg = c("1","2","3"))
barplot(eh_f2$n,names.arg = c("1","2","3"))
barplot(el_f3$n,names.arg = c("0","1"))
barplot(eh_f3$n, names.arg = c("0","1"))
par()
el_f1$n
eh_f1$n

head(eh)
head(el)
#Pick eh:C_ID_25b2509282 el:C_ID_269d816788

