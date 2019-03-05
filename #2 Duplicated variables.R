historical=read.csv("historical_transactions.csv",header = T)
train=read.csv("train.csv",header = T)
merchants=read.csv("merchants.csv", header = T)
head(historical)
approved=historical[which(historical$authorized_flag=="Y"),]
dim(approved)
tb1=approved[which(approved$card_id=="C_ID_0cd2ce025c" | approved$card_id=="C_ID_5ccc07beb9" | approved$card_id=="C_ID_c63cec2b5e" | approved$card_id=="C_ID_92420e9236"),]
tb2=approved[which(approved$card_id=="C_ID_cc3d4cd4e3" | approved$card_id=="C_ID_9f81506906" | approved$card_id=="C_ID_de14714c87" | approved$card_id=="C_ID_4de715fff3"),]
otb1=approved[which(approved$card_id=="C_ID_269d816788" | approved$card_id=="C_ID_8fa2ad405d" | approved$card_id=="C_ID_ee82cdf108" | approved$card_id=="C_ID_8186f3fcc1"),]
otb2=approved[which(approved$card_id=="C_ID_6dd41da2f6" | approved$card_id=="C_ID_52aa11c59e" | approved$card_id=="C_ID_df2a799b13" | approved$card_id=="C_ID_30e7c59658"),]

max_record_norm_tb=merge(tb1,train,by="card_id",all.x = TRUE)
dim(max_record_norm_tb)
max_record_norm_tb2=merge(tb2, train, by="card_id", all.x=TRUE)
dim(max_record_norm_tb2)
outlier_tb=merge(otb1, train, by="card_id", all.x = TRUE)
dim(outlier_tb)
outlier_tb2=merge(otb2, train, by="card_id", all.x=TRUE)
dim(outlier_tb2)

#Subset duplicated id values
library(dplyr)
id=as.data.frame(count(merchants,merchants$merchant_id))
duplicated=id[which(id$n>1),]
duplicated
dim(duplicated)
dd=merchants[merchants$merchant_id%in%duplicated$`merchants$merchant_id`,]
head(dd)
dim(dd)
#Subset Unduplicated id 
udd=merchants[!(merchants$merchant_id%in%duplicated$`merchants$merchant_id`),]
dim(udd)

#Categorical and numerical
head(dd)
udd_n=udd[,c(5:6,10,11,13,14,16,17)]#Numerical
head(udd_n)
udd_c=udd[,c(7:9,12,15,18:19,22)]#Categorical
head(udd_c)
levels(udd_c$category_1)
levels(udd_c$category_4)
levels(udd_c$most_recent_sales_range)
levels(udd_c$most_recent_purchases_range)


#Create dummy variables
uddt=udd
head(uddt)
library(dummies)
uddt$category_1= dummy(uddt$category_1)
uddt$category_4=dummy(uddt$category_4)
uddt$most_recent_sales_range=dummy(uddt$most_recent_sales_range)
uddt$most_recent_purchases_range=dummy(uddt$most_recent_purchases_range)
head(uddt)
dim(uddt)

#PCA for numerical
uddt_n=uddt[,c(5:19,22),]
dd.pca<-prcomp(na.omit(uddt_n), center=FALSE, scale. = FALSE)
summary(dd.pca)
str(dd.pca)
print(dd.pca)
plot(dd.pca,type="l")#total three components

#Select variables
dim(dd)
unique(dd$merchant_id)
dd
dim(na.omit(dd))

myd2=read.csv("the_chosen_41_merchant_id_that_had_duplicated_records.csv",header = T)
x=unique(myd2$X)
y=unique(dd$merchant_id)
myd2[which(myd2$X=="M_ID_c0b712e11a"),]
dd[which(dd$merchant_id=="M_ID_c0b712e11a"),]

