library(data.table)
d1<-fread('impute_v2_res1.csv')
d2<-fread('impute_v6_res1.csv')
setkey(d1,'id')
setkey(d2,'id')
d12<-d1[d2]
d12[,Demanda_uni_equil:=0.5*Demanda_uni_equil+0.5*i.Demanda_uni_equil]
d12[,i.Demanda_uni_equil:=NULL]

write.csv(d12,file='impute_v2_v6_res1.csv',row.names=F)