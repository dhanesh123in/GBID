rm(list=ls())
gc()

library(xgboost)
source("helper.r")

newrun=FALSE
reruncp=FALSE

if (newrun) {
  train=fread('train.csv',select = c("Semana",'Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK', 'Demanda_uni_equil'))
  test=fread('test.csv',select = c("Semana",'id','Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  

  if (reruncp) {
  #This call takes a while to run
    cdataf<-create_client_data()
    pdataf<-create_product_data()
    
    save(cdataf,pdataf,file="cpdata.Rda")
  } else {
    load(file="cpdata.Rda")
  }
  
  train_wk=9
  data_test1=create_lag_demands(test[Semana==10,],train[Semana>=5,],10,cdataf,pdataf,type='test1')
  data_test2=create_lag_demands(test[Semana==11,],train[Semana>=6,],11,cdataf,pdataf,type='test2')
  data_train=create_lag_demands(train[Semana==train_wk,],train[Semana>=(train_wk-5) & Semana<train_wk,],train_wk,cdataf,pdataf)
  
  rm(train)
  
  save(data_test1,data_test2,data_train,file="train_test.Rda")

} else {
  load(file="train_test.Rda")  
}

###########


features=names(data_train)[!(names(data_train) %in% c('Semana','Demanda_uni_equil','target','id'))]

set.seed(1205)

wltst=sample(nrow(data_train),0.3*nrow(data_train))

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features,with=FALSE]),
                  label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist<-list(dval=dval)

clf <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.85,
                               colsample_bytree=0.7) ,
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features,with=FALSE]),
                                    label=data.matrix(data_train[-wltst,target]),missing=NA), 
                 nrounds = 150, 
                 verbose = 1,
                 print.every.n=5,
                 early.stopping.rounds    = 10,
                 watchlist           = watchlist,
                 maximize            = FALSE,
                 eval_metric='rmse'
)


#########

set.seed(2407)

##Leave lag1 variables for wk11 predictions
features2=names(data_train)[!(names(data_train) %in% c('Semana','Demanda_uni_equil','target','pred1_1','pred2_1','pred3_1','id'))]

dval<-xgb.DMatrix(data=data.matrix(data_train[wltst,features2,with=FALSE]),
                  label=data.matrix(data_train[wltst,target]),missing=NA)
watchlist<-list(dval=dval)

clf2 <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.85,
                               colsample_bytree=0.7) ,
                 data = xgb.DMatrix(data=data.matrix(data_train[-wltst,features2,with=FALSE]),
                                    label=data.matrix(data_train[-wltst,target]),missing=NA), 
                 nrounds = 150, 
                 verbose = 1,
                 print.every.n=5,
                 early.stopping.rounds    = 10,
                 watchlist           = watchlist,
                 maximize            = FALSE,
                 eval_metric='rmse'
)

#################


# Make prediction for the 10th week
pred<-predict(clf,xgb.DMatrix(data.matrix(data_test1[,features,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
results=data.frame(id=data_test1$id,Demanda_uni_equil=res)

# Make prediction for the 11th week
pred<-predict(clf2,xgb.DMatrix(data.matrix(data_test2[,features2,with=FALSE]),missing=NA))
res=exp(round(pred,5))-1
res.df=data.frame(id=data_test2$id,Demanda_uni_equil=res)

#Create submission data
results=rbind(results, res.df)

results[results[,2]<0,2]=0
results[,2]=round(results[,2],1)
results[,1]=as.integer(results[,1])
class(results[,1])='int32'
options(digits=18)
write.csv(results,file='impute_v2_res1.csv',row.names=F)