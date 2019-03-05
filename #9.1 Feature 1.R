#Instaill packages
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(dummies)
library(party)

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

#Missing values
sum(is.na(f1_hist))
f1_hist=na.omit(f1_hist)
sum(is.na(f1_hist))

#Infinity
sum(is.infinite(f1_hist$card_id))
sum(is.infinite(f1_hist$authorized_flag))
sum(is.infinite(f1_hist$city_id))
sum(is.infinite(f1_hist$category_1))
sum(is.infinite(f1_hist$installments))
sum(is.infinite(f1_hist$category_3))
sum(is.infinite(f1_hist$merchant_category_id))
sum(is.infinite(f1_hist$merchant_id))
sum(is.infinite(f1_hist$month_lag))
sum(is.infinite(f1_hist$purchase_amount))
sum(is.infinite(f1_hist$purchase_date))
sum(is.infinite(f1_hist$category_2))
sum(is.infinite(f1_hist$state_id))
sum(is.infinite(f1_hist$subsector_id))
sum(is.infinite(f1_hist$first_active_month))
sum(is.infinite(f1_hist$feature_1))

#Subsetting numerical dataset
names(f1_hist)
nf1_hist=f1_hist[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_1")]
nf1_hist$feature_1=as.factor(nf1_hist$feature_1)
nf1_new=f1_new[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_1")]
nf1_new$feature_1=as.factor(nf1_new$feature_1)

#PCA on Historical (Category1, installment, category 3,3A,3B,3C)
nf1_hist$category_1=dummy(nf1_hist$category_1)
nf1_hist$category_3=dummy(nf1_hist$category_3)
nf1_hist$feature_1=dummy(nf1_hist$feature_1)
nf1_hist=na.omit(nf1_hist)
p1=prcomp(nf1_hist, center = FALSE, scale. = FALSE)
dim(train)
summary(p1)
p1_var=get_pca_var(p1)
head(p1_var$coord)

#PCA on New Merchant (Category1,1N,1Y, installments, category3,3A)
nf1_new$category_1=dummy(nf1_new$category_1)
nf1_new$category_3=dummy(nf1_new$category_3)
nf1_new$feature_1=dummy(nf1_new$feature_1)
sum(is.na(nf1_new))
nf1_new=na.omit(nf1_new)
p2=prcomp(nf1_new,center = FALSE, scale. = FALSE)
summary(p2)
p2_var=get_pca_var(p2)
head(p2_var$coord)


