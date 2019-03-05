#Import dataset
C_merchant_id=read.csv("C_merchant_id_lst.csv",header = T)
dim(C_merchant_id)
historical=read.csv("historical_transaction_cardID_C.csv",header = T)
dim(historical)
head(historical)
merchant=read.csv("merchant_tb_C.csv",header = T)
dim(merchant)
head(merchant)
new_tran=read.csv("new_transaction_cardID_C.csv",header=T)
dim(new_tran)
head(new_tran)
train=read.csv("train_C.csv",header=T)
dim(train)
head(train)
head(c)

#Feature 1
feature1=train[,1:3]
names(feature1)

#Feature 1 & Historical
f1_hist=merge(historical, feature1, on="card_id",all.x = TRUE)
head(f1_hist)

#Feature 1 & New Transactional
f1_new=merge(new_tran,feature1,on="card_id",all.x = TRUE)
head(f1_new)

#Type of features
names(f1_hist)
sum(is.na(f1_hist))
sum(is.na(f1_new))
typeof(f1_hist$card_id)
typeof(f1_hist$authorized_flag)
typeof(f1_hist$city_id)
typeof(f1_hist$category_1)
typeof(f1_hist$category_2)
typeof(f1_hist$category_3)
typeof(f1_hist$installments)
typeof(f1_hist$merchant_category_id)
typeof(f1_hist$merchant_id)
typeof(f1_hist$month_lag)
typeof(f1_hist$purchase_amount)
typeof(f1_hist$purchase_date)
typeof(f1_hist$state_id)
typeof(f1_hist$subsector_id)
typeof(f1_hist$first_active_month)
typeof(f1_hist$feature_1)

#Subsetting numerical data
f1_hist_n=f1_hist[,c(2,4:6,9:12,15:16)]
names(f1_hist_n)
sum(is.na(f1_hist_n))
plot(f1_hist_n$authorized_flag)
plot(f1_hist$category_1)
plot(f1_hist$installments)
plot(f1_hist$category_3)
plot(f1_hist$month_lag)
plot(f1_hist$purchase_amount)
plot(f1_hist$purchase_date)
plot(f1_hist$first_active_month)
plot(f1_hist$feature_1)

#Random Forest
f1_hist$feature_1=as.character(f1_hist$feature_1)
typeof(f1_hist$feature_1)
f1_hist$feature_1=as.factor(f1_hist$feature_1)
library(party)
cf1<-cforest(as.factor(unlist(feature1)~.,data=f1_hist)



