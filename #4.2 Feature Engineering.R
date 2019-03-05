#Import dataset
myd1=read.csv("1.csv", header = T)
train=read.csv("train.csv", header = T)

#Split myd1
myd1.size=round(0.1*nrow(myd1))
id.myd1=sample(1:nrow(myd1),myd1.size, replace=FALSE)
train.set=myd1[id.myd1,]
test.set=myd1[-id.myd1,]

#Split train
train.size=round(0.1*nrow(train))
id.train=sample(1:nrow(train), train.size, replace=FALSE)
train.train=train[id.train,]
train.test=train[-id.train,]

#Redundant features
library(mlbench)
library(caret)
head(myd1)
dim(myd1)
var=myd1[c,6:45]
dim(var)
correlationMatrix<-cor(var)
highlyCorrelated<-findCorrelation(correlationMatrix,cutoff = 0.5)
print(highlyCorrelated)

correlationMatrix2<-cor(train[,3:6])
hc<-findCorrelation(correlationMatrix2,cutoff = 0.7)
print(hc)

#Rank features by importantce
library(mlbench)
library(caret)
train.train$target=as.factor(train.train$target)
control<-trainControl(method="repeatedcv",number=2, repeats=1)
model<-train(target~., data=train.train, method="lvq",trcontrol=control)
head(train.train)
