library(data.table)
d1<-fread('impute_v2_res1.csv')
d2<-fread('impute_v6_res1.csv')
d3<-fread('results2.csv')
d1[,d1:=Demanda_uni_equil]
d2[,d2:=Demanda_uni_equil]
d3[,d3:=Demanda_uni_equil]
d1[,Demanda_uni_equil:=NULL]
d2[,Demanda_uni_equil:=NULL]
d3[,Demanda_uni_equil:=NULL]
setkey(d1,'id')
setkey(d2,'id')
d12<-d1[d2]
setkey(d12,'id')
setkey(d3,'id')
d123<-d12[d3]

d123[,Demanda_uni_equil:=0.34*d1+0.33*d2+0.33*d3]
d123[,d1:=NULL]
d123[,d2:=NULL]
d123[,d3:=NULL]

write.csv(d123,file='impute_v2_v6_results2_res1.csv',row.names=F)