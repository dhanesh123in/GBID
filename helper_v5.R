library(data.table)
library(stringr)

#Ported from https://www.kaggle.com/abbysobh/grupo-bimbo-inventory-demand/classifying-client-type-using-client-names/code
client_map <- function(x) {
  
  y=16
  if (grepl('.*REMISION.*',x,perl=TRUE,useBytes=TRUE)) {y=1}
  else if (grepl('.*WAL MART.*|.*SAMS CLUB.*',x,perl=TRUE,useBytes=TRUE)) {y=2}
  else if (grepl('.*OXXO.*',x,perl=TRUE,useBytes=TRUE)) {y=3}
  else if (grepl('.*CONASUPO.*',x,perl=TRUE,useBytes=TRUE)) {y=4}
  else if (grepl('.*BIMBO.*',x,perl=TRUE,useBytes=TRUE)) {y=5}
  else if (grepl('.*COLEG.*|.*UNIV.*|.*ESCU.*|.*INSTI.*|.*PREPAR.*',x,perl=TRUE,useBytes=TRUE)) {y=6}
  else if (grepl('.*PUESTO.*',x,perl=TRUE,useBytes=TRUE)) {y=7}
  else if (grepl('.*FARMA.*|.*HOSPITAL.*|.*CLINI.*',x,perl=TRUE,useBytes=TRUE)) {y=8}
  else if (grepl('.*CAFE.*|.*CREMERIA.*|.*DULCERIA.*|.*REST.*|.*BURGER.*|.*TACO.*|.*TORTA.*|.*TAQUER.*|.*HOT DOG.*|.*COMEDOR.*|.*ERIA.*|.*BURGU.*', x)) {y=9} 
  else if (grepl('.*SUPER.*',x,perl=TRUE,useBytes=TRUE)) {y=10}
  else if (grepl('.*COMERCIAL.*|.*BODEGA.*|.*DEPOSITO.*|.*ABARROTES.*|.*MERCADO.*|.*CAMBIO.*|.*MARKET.*|.*MART\\s.*|.*MINI\\s.*|.*PLAZA.*|.*MISC.*|.*ELEVEN.*|.*EXP.*|.*SNACK.*|.*PAPELERIA.*|.*CARNICERIA.*|.*LOCAL.*|.*COMODIN.*|.*PROVIDENCIA.*',x,perl=TRUE,useBytes=TRUE)) {y=11}
  else if (grepl('.*VERDU.*|.*FRUT.*',x,perl=TRUE,useBytes=TRUE)) {y=12}
  else if (grepl('.*HOTEL.*|.*MOTEL.*',x,perl=TRUE,useBytes=TRUE)) {y=13}
  else if (grepl('.*LA\\s.*|.*EL\\s.*|.*DE\\s.*|.*LOS\\s.*|.*DEL\\s.*|.*Y\\s.*|.*SAN\\s.*|.*SANTA\\s.*|.*AG\\s.*|.*LAS\\s.*|.*MI\\s.*|.*MA\\s.*|.*II.*|.*[0-9]+.*',x,perl=TRUE,useBytes=TRUE)) {y=14}
  else if (x==toupper(x) || x != "NO IDENTIFICADO") {y= 15}
  
  y
}

create_client_data<-function() {
  clientdata=fread('cliente_tabla.csv')
  
  clientdata[,NombreCliente:=lapply(.SD, function(x) gsub("\\s\\s+"," ",x)), by=.(Cliente_ID)]
  
  cdata=unique(clientdata)
  
  setkey(cdata,Cliente_ID)
  #cdata$ClientN1=0
  cdata[,ClientN1:=lapply(.SD,client_map) ,.SDcols=c("NombreCliente"),by=.(Cliente_ID)]
  cdataf=cdata[,.(ClientN=min(ClientN1)),by=.(Cliente_ID)]
  cdataf
}

product_extract <- function(x) {
  x=tolower(x)
  sname=trimws(sub("(^\\D*).*","\\1",x))
  pieces=as.numeric(sub(".*?(\\d+)p.*","\\1",x))
  wgt=as.numeric(sub(".*?(\\d+)(g|kg|ml|oz)\\s.*","\\1",x))
  units=sub(".*?\\d+(g|kg|ml|oz)\\s.*","\\1",x)
  if (units==x) {units=""}
  brand=sub(".*\\s([a-z]+)\\s\\d+$","\\1",x)
  if (brand==x) {brand=""}
  
  list(sname=sname,pieces=pieces,wgt=wgt,units=units,brand=brand)
}

create_product_data<-function() {
  productdata=fread('producto_tabla.csv')
  pdataf=productdata[,sapply(.SD,product_extract),.SDcols=c("NombreProducto"),by=.(Producto_ID)]
  pdataf$V1<-as.numeric(as.factor(pdataf$V1))
  pdataf$V4<-as.numeric(as.factor(pdataf$V4))
  pdataf$V5<-as.numeric(as.factor(pdataf$V5))
  
  pdataf
}

create_agency_data<-function() {
	adataf=fread('town_state.csv')
	adataf$Town<-as.numeric(as.factor(adataf$Town))
	adataf$State<-as.numeric(as.factor(adataf$State))
	adataf
}

create_lag_demands <- function (data_src,data_gen,src_wk,cdataf,pdataf,adataf,type='train') {
  
  # if (type!='test2') {data_src=merge(data_src,data_gen[Semana==(src_wk-1),.(pred1_1=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))}
  # data_src=merge(data_src,data_gen[Semana==(src_wk-2),.(pred1_2=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-3),.(pred1_3=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-4),.(pred1_4=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-5),.(pred1_5=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-6),.(pred1_6=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-7),.(pred1_7=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-8),.(pred1_8=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID,Ruta_SAK)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID', 'Ruta_SAK'))
  
# #   ##WIP
# #   if (type!='test2') {
# #     data_src[,.(pred1_3ma_5:=mean(c(pred1_8,pred1_7,pred1_6),na.rm=T))]
# #     
# #   } else {
# #   }
# #   
  # if (type!='test2') {data_src=merge(data_src,data_gen[Semana==(src_wk-1),.(pred2_1=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))}
  # data_src=merge(data_src,data_gen[Semana==(src_wk-2),.(pred2_2=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-3),.(pred2_3=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-4),.(pred2_4=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-5),.(pred2_5=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-6),.(pred2_6=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-7),.(pred2_7=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  # data_src=merge(data_src,data_gen[Semana==(src_wk-8),.(pred2_8=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID,Agencia_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID', 'Agencia_ID'))
  
  if (type!='test2') {data_src=merge(data_src,data_gen[Semana==(src_wk-1),.(pred3_1=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))}
  data_src=merge(data_src,data_gen[Semana==(src_wk-2),.(pred3_2=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-3),.(pred3_3=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-4),.(pred3_4=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-5),.(pred3_5=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-6),.(pred3_6=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-7),.(pred3_7=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))
  data_src=merge(data_src,data_gen[Semana==(src_wk-8),.(pred3_8=log1p(mean(Demanda_uni_equil))),by=.(Cliente_ID,Producto_ID)],all.x=T,by=c('Cliente_ID', 'Producto_ID'))

  data_src[,pred3_3ma_1:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_2","pred3_3","pred3_4")]
  data_src[,pred3_3ma_2:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_3","pred3_4","pred3_5")]
  data_src[,pred3_3ma_3:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_4","pred3_5","pred3_6")]
  data_src[,pred3_3ma_4:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_5","pred3_6","pred3_7")]
  data_src[,pred3_3ma_5:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_6","pred3_7","pred3_8")]
  
  data_src[,pred3_4ma_1:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_2","pred3_3","pred3_4","pred3_5")]
  data_src[,pred3_4ma_2:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_3","pred3_4","pred3_5","pred3_6")]
  data_src[,pred3_4ma_3:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_4","pred3_5","pred3_6","pred3_7")]
  data_src[,pred3_4ma_4:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_5","pred3_6","pred3_7","pred3_8")]
  
  data_src[,pred3_5ma_1:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_2","pred3_3","pred3_4","pred3_5","pred3_6")]
  data_src[,pred3_5ma_2:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_3","pred3_4","pred3_5","pred3_6","pred3_7")]
  data_src[,pred3_5ma_3:=rowMeans(.SD,na.rm=TRUE),.SDcols=c("pred3_4","pred3_5","pred3_6","pred3_7","pred3_8")]
  
  
  nAgencia_ID=data_src[,.(nAgencia_ID=.N),by=.(Agencia_ID,Semana)]
  nAgencia_ID=nAgencia_ID[,.(nAgencia_ID=mean(nAgencia_ID,na.rm=T)),by=Agencia_ID]
  data_src=merge(data_src,nAgencia_ID,by='Agencia_ID',all.x=T)
  nRuta_SAK=data_src[,.(nRuta_SAK=.N),by=.(Ruta_SAK,Semana)]
  nRuta_SAK=nRuta_SAK[,.(nRuta_SAK=mean(nRuta_SAK,na.rm=T)),by=Ruta_SAK]
  data_src=merge(data_src,nRuta_SAK,by='Ruta_SAK',all.x=T)
  nCliente_ID=data_src[,.(nCliente_ID=.N),by=.(Cliente_ID,Semana)]
  nCliente_ID=nCliente_ID[,.(nCliente_ID=mean(nCliente_ID,na.rm=T)),by=Cliente_ID]
  data_src=merge(data_src,nCliente_ID,by='Cliente_ID',all.x=T)
  nProducto_ID=data_src[,.(nProducto_ID=.N),by=.(Producto_ID,Semana)]
  nProducto_ID=nProducto_ID[,.(nProducto_ID=mean(nProducto_ID,na.rm=T)),by=Producto_ID]
  data_src=merge(data_src,nProducto_ID,by='Producto_ID',all.x=T)
  
#   data_src1<-data_src[,.(nRpA=length(unique(Ruta_SAK)),nCpA=length(unique(Cliente_ID)),nPpA=length(unique(Producto_ID))),by=.(Semana,Agencia_ID)]
#   data_src<-merge(data_src,data_src1,all.x=T,by=c("Semana","Agencia_ID"))
#   
#   data_src1<-data_src[,.(nRpP=length(unique(Ruta_SAK)),nCpP=length(unique(Cliente_ID)),nApP=length(unique(Agencia_ID))),by=.(Semana,Producto_ID)]
#   data_src<-merge(data_src,data_src1,all.x=T,by=c("Semana","Producto_ID"))
#   
#   data_src1<-data_src[,.(nRpC=length(unique(Ruta_SAK)),nPpC=length(unique(Producto_ID)),nApC=length(unique(Agencia_ID))),by=.(Semana,Cliente_ID)]
#   data_src<-merge(data_src,data_src1,all.x=T,by=c("Semana","Cliente_ID"))
#   
#   data_src1<-data_src[,.(nCpR=length(unique(Cliente_ID)),nPpR=length(unique(Producto_ID)),nApR=length(unique(Agencia_ID))),by=.(Semana,Ruta_SAK)]
#   data_src<-merge(data_src,data_src1,all.x=T,by=c("Semana","Ruta_SAK"))
  
  ##############
  if (type=='train') {data_src$target=log(data_src$Demanda_uni_equil+1)}
  
  data_src=merge(data_src,cdataf[,.(Cliente_ID,ClientN)],all.x=T,by=c('Cliente_ID'))
  
  data_src=merge(data_src,pdataf[,.(Producto_ID,V1,V2,V3,V4,V5)],all.x=T,by=c('Producto_ID'))
  
  data_src=merge(data_src,adataf[,.(Agencia_ID,Town,State)],all.x=T,by=c('Agencia_ID'))
  
  data_src
}
