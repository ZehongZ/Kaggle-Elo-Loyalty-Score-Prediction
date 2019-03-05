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

#Subset feature 2
feature2=train[,c(1:2,4)]
names(feature2)

#Feature 2 & Historical
f2_hist=merge(historical, feature2, on="card_id",all.x = TRUE)
head(f2_hist)

#Feature 2 & New Transactional
f2_new=merge(new_tran,feature2,on="card_id",all.x = TRUE)
head(f2_new)

#Missing values
sum(is.na(f2_hist))
f2_hist=na.omit(f2_hist)
sum(is.na(f2_hist))
names(f2_hist)
#Subsetting numerical dataset
names(f2_hist)
nf2_hist=f2_hist[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_2")]
nf2_hist$feature_2=as.factor(nf2_hist$feature_2)
nf2_new=f2_new[,c("category_1","installments","category_3","month_lag","purchase_amount","category_2","feature_2")]
nf2_new$feature_2=as.factor(nf2_new$feature_2)

#Infinity test
sum(is.infinite(nf2_hist$category_1))
sum(is.infinite(nf2_hist$installments))
sum(is.infinite(nf2_hist$category_3))
sum(is.infinite(nf2_hist$month_lag))
sum(is.infinite(nf2_hist$purchase_amount))
sum(is.infinite(nf2_hist$category_2))
sum(is.infinite(nf2_hist$feature_2))

#PCA on Historical (Category1, installment, category 3,3A,3B,3C)
nf2_hist$category_1=dummy(nf2_hist$category_1)
nf2_hist$category_3=dummy(nf2_hist$category_3)
nf2_hist$feature_2=dummy(nf2_hist$feature_2)
nf2_hist=na.omit(nf2_hist)
p1=prcomp(nf2_hist, center = FALSE, scale. = FALSE)
summary(p1)
p1_var=get_pca_var(p1)
head(p1_var$coord)


#PCA on New Merchant (Category1,1N,1Y, installments, category3,3A)
nf2_new$category_1=dummy(nf2_new$category_1)
nf2_new$category_3=dummy(nf2_new$category_3)
nf2_new$feature_2=dummy(nf2_new$feature_2)
sum(is.na(nf2_new))
nf2_new=na.omit(nf2_new)
p2=prcomp(nf2_new,center = FALSE, scale. = FALSE)
summary(p2)
p2_var=get_pca_var(p2)
head(p2_var$coord)

