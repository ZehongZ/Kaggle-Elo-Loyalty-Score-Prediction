#Instaill packages
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(dummies)

#Input datasets
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

#Subset feature 3
feature3=train[,c(1:2,5)]
names(feature3)

#Feature 3 & Historical
f3_hist=merge(historical, feature3, on="card_id",all.x = TRUE)
head(f3_hist)

#Feature 2 & New Transactional
f3_new=merge(new_tran,feature3,on="card_id",all.x = TRUE)
head(f3_new)

#Missing values
sum(is.na(f3_hist))
f3_hist=na.omit(f3_hist)
sum(is.na(f3_hist))
names(f3_hist)

#Subsetting numerical dataset
#Subsetting numerical dataset
names(f3_hist)
nf3_hist=f3_hist[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_3")]
nf3_hist$feature_3=as.factor(nf3_hist$feature_3)
nf3_new=f3_new[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_3")]
nf3_new$feature_3=as.factor(nf3_new$feature_3)

#PCA on Historical (Category1, installment, category3 3A,3B,3C)
nf3_hist$category_1=dummy(nf3_hist$category_1)
nf3_hist$category_3=dummy(nf3_hist$category_3)
nf3_hist$feature_3=dummy(nf3_hist$feature_3)
nf3_hist=na.omit(nf3_hist)
p1=prcomp(nf3_hist, center = FALSE, scale. = FALSE)
summary(p1)
p1_var=get_pca_var(p1)
head(p1_var$coord)

#PCA on New Merchant (Category1,1N,1Y, installments, category3,3A)
nf3_new$category_1=dummy(nf3_new$category_1)
nf3_new$category_3=dummy(nf3_new$category_3)
nf3_new$feature_3=dummy(nf3_new$feature_3)
sum(is.na(nf3_new))
nf3_new=na.omit(nf3_new)
p2=prcomp(nf3_new,center = FALSE, scale. = FALSE)
summary(p2)
p2_var=get_pca_var(p2)
head(p2_var$coord)

