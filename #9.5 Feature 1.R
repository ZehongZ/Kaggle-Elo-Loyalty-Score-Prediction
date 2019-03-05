#Instaill packages
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)
library(dummies)
library(party)
library(data.table)
library(mlbench)
library(caret)
library(rpart)
library(e1071)
library(klaR)
devtools::install_github("hoxo-m/githubinstall")

#Input data
historical=data.table::fread("historical_transactions.csv",header=T)
merchants=data.table::fread("1.csv",header = T)
new=data.table::fread("new_merchant_transactions.csv",header = T)
train=data.table::fread("train.csv",header = T)
#Rename column name
colnames(merchants)[colnames(merchants)=="V1"]<-"merchant_id"
names(merchants)

#Train Control
trainControl=trainControl(method="cv",number=5)

#Screen out outliers for train dataset
dim(train)
train=train[which(train$target>=-1.59),]
dim(train)
train=train[which(train$target<=3.25),]
dim(train)

#Subsetting training dataset
trainIndex=createDataPartition(historical$card_id,p=0.001,list=FALSE)
htrain=historical[trainIndex,]
train=train[train$card_id%in%htrain$card_id]
sum(is.na(htrain))
merchants=merchants[merchants$merchant_id%in%htrain$merchant_id]
sum(is.na(merchants))
sum(is.na(historical))

#Subsetting test dataset
htest=historical[-trainIndex,]
test=train[train$card_id%in%htest$card_id]
merchants_test=merchants[merchants$merchant_id%in%htest$merchant_id]

#Dimension of datasets
dim(htrain)
dim(train)
dim(merchants)

#Merge datasets
shop=merge(htrain,merchants,by.x="merchant_id",by.y="merchant_id",all.x = TRUE)
shop=merge(htrain,merchants, on="merchant_id",all.x=TRUE)
shopt=merge(htest,merchants_test,by.x="merchant_id",by.y="merchant_id",all.x=TRUE)
dim(shop)
names(shop)
sum(is.na(shop))
#Merge with Feature_1
feature1=train[,1:3]
f1=merge(shop, feature1, by.x="card_id", by.y="card_id",all.x = TRUE)
names(f1)

#Merge with Feature_2
feature2=train[,c(1:2,4)]
f2=merge(shop, feature2, by.x="card_id",by.y="card_id",all.x = TRUE)
names(f2)

#Merge with Feature_3
feature3=train[,c(1:2,5)]
f3=merge(shop, feature3, by.x="card_id",by.y="card_id",all.x=TRUE)
names(f3)

#Merge with all
all=merge(shop, train, by.x="card_id",by.y="card_id",all.x = TRUE)
allt=merge(shopt,train, by.x="card_id",by.y="card_id",all.x = TRUE)
dim(all)
names(all)
all$target=as.factor(all$target)
head(all$target)
allt$target=as.factor(allt$target)
sum(is.na(all))
dim(all)

#Subsetting numeric data
f1n=f1[,c(3,5:7,9,10:12,19:60)]
f2n=f2[,c(3,5:7,9,10:12,19:60)]
f3n=f3[,c(3,5:7,9,10:12,19:60)]
alln=all[,c(3,5:7,9,10:12,19:63)]
dim(f1n)
dim(f2n)
dim(f3n)
dim(alln)
names(f1n)
names(f2n)
names(f3n)
names(alln)

#PCA
sum(is.na(f1n))
f1na=is.na(f1n)
dim(f1na)
p1=prcomp(f1na,center = FALSE, scale. = FALSE)
#Scree Plot
fviz_eig(p1)
summary(p1)#Require 2 components(77.06%,92.78%)
print(p1)
eig.val<-get_eigenvalue(p1)
eig.val
pca_var=get_pca_var(p1)
head(pca_var$coord)#Authorized_flag, category_1, installments, category_3,month_lag,purchase_amount
head(pca_var$cos2)
head(pca_var$contrib)

sum(is.na(f2n))
f2na=is.na(f2n)
p2=prcomp(f2na, center = TRUE, scale. = TRUE)
summary(p2)#Require 2 components
eig.val2<-get_eigenvalue(p2)
eig.val2
pca_var2=get_pca_var(p2)
head(pca_var2$coord)#Authorized_flag, category_1, installments, category_3,month_lag,purchase_amount
head(pca_var2$cos2)
head(pca_var2$contrib)

sum(is.na(f3n))
f3na=is.na(f3n)
p3=prcomp(f3na, center = FALSE, scale. = FALSE)
summary(p3)#Require 2 components
eig.val3<-get_eigenvalue(p3)
eig.val3
pca_var3=get_pca_var(p3)
head(pca_var3$coord)#Authorized_flag, category_1,installments, category_3,month_lag, purchase_amount
head(pca_var3$cos2)
head(pca_var3$contrib)

sum(is.na(alln))
allna=is.na(alln)
pa=prcomp(alln,center=FALSE, scale. = FALSE)
summary(pa)#Require 2 components
eig.vala<-get_eigenvalue(pa)
eig.vala
pca_vara=get_pca_var(pa)
head(pca_vara$coord)#Authroized_flag, category_1, installments, category_3, month_lag, purchase_amount

#RFE
library(mlbench)
library(caret)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(all[,1:62], all[,63], sizes=c(1:62), rfeControl=control)

data("PimaIndiansDiabetes")
length(PimaIndiansDiabetes[,1:8])
length(PimaIndiansDiabetes[,9])
dim(PimaIndiansDiabetes)
#myd
names(all)
myd=all[,c(3,5,6,7,9,10,63)]
myd
write.csv(myd, file="myd.csv")
str(myd)
myd$authorized_flag=dummy(myd$authorized_flag)
myd$category_1=dummy(myd$category_1)
myd$category_3=dummy(myd$category_3)
head(myd)
myd$authorized_flag=as.integer(myd$authorized_flag)
myd$category_1=as.integer(myd$category_1)
myd$category_3=as.integer(myd$category_3)
sum(is.na(myd))
dim(myd)
158607/325744
#Mydt
mydt=allt[,c(3,5,6,7,9,10,63)]
write.csv(mydt, file="mydt.csv")
mydt$authorized_flag=dummy(mydt$authorized_flag)
mydt$category_1=dummy(mydt$category_1)
mydt$category_3=dummy(mydt$category_3)
mydt$authorized_flag=as.integer(mydt$authorized_flag)
mydt$category_1=as.integer(mydt$category_1)
mydt$category_3=as.integer(mydt$category_3)

#Correlation
cor(myd)
cor(mydt)

#Linear regression
lmyd=lm(myd$target~.,myd)
summary(lmyd)

#Logistic regression
library(VGAM)
fit<-vglm(myd$target~., family = multinomial, data=myd)

#Linear Discriminant Regression
library(MASS)
library(mlbench)
fit=lda(myd$target~.,data=myd)
summary(fit)
print(fit)
predictions=predict(fit, myd[,1:6])$class

#GLM (Can only be binary class)
model=glm(myd$target~., data=myd, family=binomial(link=logit))


#Regression Tree
fit<-rpart(myd$target~.,method = "anova",data=myd)
printcp(fit)
summary(fit)
plotcp(fit)

#Decision Tree
fit2<-rpart(all$target~., method="class",data=all)


#Light GBM
library(Matrix)
library(MLmetrics)
library(devtools)
options(devtools.install.args = "--no-multiarch")
install_git("https://github.com/Microsoft/LightGBM", subdir = "R-package")

#Linear Support Vector Machine
library(caret)
library(kernlab)
myd1$target=as.factor(myd1$target)
myd1=is.na(myd)
sum(is.na(myd))
myd=is.na(myd)
sum(is.na(myd))
grid=expand.grid(C=c(0.1,0.5,1,1.5,2))
svm_linear=train(myd1[["target"]]~., data=myd1,method="svmRadial",trControl=trainControl,preProcess=c("center","scale"),tuneGrid=grid,tuneLength=10)

#Xgboost, can only work binary class situation
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
library(xgboost)
library(archdata)
library(caret)
library(dplyr)
library(Ckmeans.1d.dp)
dim(myd)
data=myd[,1:6]
label=myd[,7]
data_matrix<-xgb.DMatrix(data=as.matrix(data),label=as.matrix(label))
numofClass<-length(unique(myd$target))
xgb_params<-list("objective"="multi:softprob",
                 "eval_metric"="mlogloss",
                 "num_class"=numofClass)
nround<-5
cv.nfold<-3
cv_model<-xgb.cv(params=xgb_params,
                 data=data_matrix,
                 nrounds=nround,
                 nfold=cv.nfold,
                 verbose=FALSE,
                 prediction=TRUE)

#Gradient boosting 
library(gbm)
library(MASS)
str(myd)

train.boost=gbm(myd2$target~., data=myd2[,1:6], distribution="gaussian",n.trees=10,shrinkage=0.01,interaction.depth = 4)
