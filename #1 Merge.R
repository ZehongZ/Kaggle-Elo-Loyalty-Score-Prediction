#Import a dataset
historical=read.csv("historical_transactions.csv", header=T)
train=read.csv("train.csv",header=T)
new_merchant=read.csv("new_merchant_transactions.csv",header = T)
#Dimension of datasets
dim(historical)
dim(train)
#First 5 rows
head(myd)
head(myd$authorized_flag,100) #Authorized_flag has two levels
head(myd$city_id,100)
head(myd$category_1,100)
head(myd$category_2,100)#Contain missing values
head(myd$installments,100)
head(myd$category_3,100)
#Distribution of each variable
hist(myd$month_lag)#Not normally distributed
qqnorm(myd$month_lag)
hist(myd$purchase_amount,xlim = c(-2,10),ylim = c(0,10))
head(myd$purchase_amount,100)
range(myd$purchase_amount)
#Split into training and testing
library(rpart)
historical.size=round(0.66*nrow(historical))
id.historical=sample(1:nrow(historical),historical.size, replace=FALSE)
train.set=historical[id.historical,]
test.set=historical[-id.historical,]
dim(train.set)
dim(test.set)
head(train.set)
#Screen out card_id
train$card_id %in% train.set$card_id
total<-merge(train.set,train, by="card_id", all.y = TRUE)
head(total)
dim(total)
head(historical)
dim(historical)
is.null(total)
head(total,100)
#Check  Missing Values
is.null(total)
library(naniar)
gg_miss_upset(total)

#Frequency
library(plyr)
approved=total[which(total$authorized_flag=="Y"),]
head(approved)
dim(approved)
count=as.data.frame(count(approved$card_id))
count.sort=count[order(count$freq,decreasing = T),]
head(count.sort)
#Subset 4 most frequent cardID
Fmost=approved[which(approved$card_id=="C_ID_0cd2ce025c"),]
head(Fmost)
dim(Fmost)
Smost=approved[which(approved$card_id=="C_ID_cc3d4cd4e3"),]
head(Smost)
dim(Smost)
Tmost=approved[which(approved$card_id=="C_ID_5ccc07beb9"),]
head(Tmost)
dim(Tmost)
Fomost=approved[which(approved$card_id=="C_ID_9f81506906"),]
head(Fomost)
dim(Fomost)

write.csv(Fmost,"C_ID_0cd2ce025c")
write.csv(Smost,"C_ID_cc3d4cd4e3")
write.csv(Tmost, "C_ID_5ccc07beb9")
write.csv(Fomost,"C_ID_9f81506906")

approved2=historical[which(historical$authorized_flag=="Y"),]
dim(approved2)
