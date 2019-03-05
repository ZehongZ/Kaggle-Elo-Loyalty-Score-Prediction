tr=tr[,c(4,26,46,12,25,14,15,30)]
ts=ts[,c(4,26,46,12,25,14,15,30)]
trLabel=as.numeric(train$target)
tsLabel=as.numeric(test$target)
new_tr=model.matrix(~.+0, data=tr,with=F)
new_ts=model.matrix(~.+0, data=ts, with=F)
head(new_tr)
head(new_ts)

dtrain<-xgb.DMatrix(data=new_tr, label=trLabel)
dtest<-xgb.DMatrix(data=new_ts, label=tsLabel)

#Parameters setting
params<-list(booster="dart",object="multi:softmax",eta=0.3,gamma=0,max_depth=6, min_child_weight=1, subsample=1,colsample_bytree=1,alpha=1)
#Booster: gbtree, gbliner or dart. Regression can use any
#nthread: activates parallel computation. Generally don't change
#eta: Control learning rate. Lower eta to slower computation
#gamma: Controls regularization. 
#max_depth: Controls depth of the tree
#min_child_weight: In regression, it refers to the minimum number of instances required in a child node
#subsample: Controls the number of samples supplied to a tree. Generally [0.5-0.8]
#colsample: Controls the number of feature supplied to a tree. [0.5-0.9]
#lambda: Controls L2 regularization (Ridge regression) on weights. It is used to avoid overfitting
#Alpha: Controls L1 regularization on weights. Also results in feature selectin.



#Using inbuilt xgb.cv for best nround
xgbcv<-xgb.cv(parames=params, data=dtrain, nrounds = 100, nfold=5, showsd=T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F,eval_metric="rmse")
#iteration = 9 is the best
min(xgbcv$test.error.mean)

#Build xgboost model
xgb1<-xgb.train(params = params, data=dtrain, nrounds=7,watchlist = list(val=dtest,train=dtrain),print_every_n = 10,early.stop.rounds=10,maximize = F, eval_metric="rmse")
#val-rmse:3.878798, train-rmse:3.630838
summary(xgb1)

#Prediction
xgbpred<-predict(xgb1,dtest)
xgbpred

#Inforamtion extraction
lbel=getinfo(dtest,"label")
err<-as.numeric(sum(as.integer(xgbpred>0.5) !=tsLabel))/length(tsLabel)
print(paste("test-error=",err))

#Model evaluation
actuals_preds<-data.frame(cbind(actuals=tsLabel,predicteds=xgbpred))
correlation_accuracy<-cor(actuals_preds)
print(correlation_accuracy)
head(actuals_preds)

