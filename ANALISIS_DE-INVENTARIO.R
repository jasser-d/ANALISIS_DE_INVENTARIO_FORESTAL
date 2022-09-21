#librerias urilizadas

library(tcltk)
library(asbio)
library(dplyr)
library(ggplot2)
library(readxl)
library(plotrix)

source(file = "SCRIPS/CLASS_DAP_F.R")
source(file = "SCRIPS/IVI.R")
source(file = "SCRIPS/EST_INVENTARIO.R")

#CARGAR DOCUMENTOS DE ANALISIS CON FORMATO DE:

#FORMATO DE COLUMNAS:ORDEN-PM-N_COMUN-N_CIENTIFICO-LC-HC
#LC=LARGO DE CIRCUNFERENCIA   HC=ALTURA COMERCIAL   PM=PARCELA DE MUESTREO
#____________________________________________________________________________________________

#INTRODUCIR VARIABLES DEL INVENTARIO
DATOS_INV<-cbind(190,0.7,900,0.1,0.95) %>% data.frame()
colnames(DATOS_INV)<-c("N_PARC","FACTOR_F","AREA_T","PORCEN_MUEST","NIVEL_DE_SIGNIFICANCIA")
#DIRECCION DE ARCHIVO DE INVENTARIO Y DE ALMACENAMIENTO DE RESLTADOS
RUTAS_ARCH<-c("DATOS_DE_EJEMPLO/INVENTARIO FORESTAL DE 900 Has.csv",
              "RESULTADOS_DE_EJEMPLO/")

DATA_BASE<-read.csv(file = RUTAS_ARCH[1],header = T,sep = ";",dec = ",")

#_____________________________________________________________________________________________

#CALCULO DE DAP,AREA BASAL VOLUMEN Y TABLA DE RESUMEN POR PARCELAS
#_____________________________________________________________________________________________

DAP<-(DATA_BASE$LC/(pi))
AREA_B<-((DAP/100)^2)*(pi/4)
VOL_M3<-AREA_B*DATA_BASE$HC*0.7
CALC_VAR<-cbind(DAP,AREA_B,VOL_M3)
colnames(CALC_VAR)<-c("DAP","AB","VOL")
DATA_BASE<-data.frame(DATA_BASE,CALC_VAR)

RESUMEN_PARC<-group_by(DATA_BASE,PM) %>% summarise(N_ARB=n(),AB=sum(AB),VOL=sum(VOL))

GUARDADO_FUN<-function(RUTA,NOMBRE,TABLA){
  write.table(TABLA,file = paste(RUTA,NOMBRE,".csv",sep = ""),sep = ";",dec = ",",row.names = F,na = "")
}

GUARDADO_FUN(RUTAS_ARCH[2],"RESUMEN_PARC",RESUMEN_PARC)

rm(DAP,AREA_B,VOL_M3,CALC_VAR)

#_____________________________________________________________________________________________

#ESTADISTICA DEL INVENTARIO
#_____________________________________________________________________________________________

EST_AB<-EST_DESCR(RESUMEN_PARC$AB,DATOS_INV$NIVEL_DE_SIGNIFICANCIA)
colnames(EST_AB)[2]<-"EST_AB"
EST_VOL<-EST_DESCR(RESUMEN_PARC$VOL,DATOS_INV$NIVEL_DE_SIGNIFICANCIA)
colnames(EST_VOL)[2]<-"EST_VOL"
TABLA_EST<-merge(x=EST_AB,y=EST_VOL)

GUARDADO_FUN(RUTAS_ARCH[2],"TABLA_EST",TABLA_EST)

rm(EST_AB,EST_VOL)

#___________________________________________________________________________________________

#PROYECCION DE INVENTARIO 
#___________________________________________________________________________________________
AREA_PARCEL<-DATOS_INV$AREA_T*DATOS_INV$PORCEN_MUEST/nrow(RESUMEN_PARC)



EST_MAX<-((TABLA_EST[13,3]+TABLA_EST[5,3])/AREA_PARCEL)*DATOS_INV$AREA_T
EST_MED<-((TABLA_EST[13,3])/AREA_PARCEL)*DATOS_INV$AREA_T
EST_MIN<-((TABLA_EST[13,3]-TABLA_EST[5,3])/AREA_PARCEL)*DATOS_INV$AREA_T

TABLA_PRO<-data.frame("VOLUMEN",EST_MAX,EST_MED,EST_MIN)

GUARDADO_FUN(RUTAS_ARCH[2],"PROYECCION_INVENTARIO",TABLA_PRO)

rm(EST_MED,EST_MAX,EST_MIN,AREA_PARCEL,TABLA_PRO)

#___________________________________________________________________________________________

#TABLA DE CLACIFICACION DIAMETRICA
#___________________________________________________________________________________________

CLACIFICACION<-select(DATA_BASE,N_COMUN, N_CIENTIFICO ,DAP ,AB ,VOL)
CLACIFICACION<-CLASS_DAP_30(CLACIFICACION)
GUARDADO_FUN(RUTAS_ARCH[2],"TABLA_DIAMETRICA",CLACIFICACION)

#___________________________________________________________________________________________

#TABLA DE INDICE DE VALOR DE IMPROTANCIA
#___________________________________________________________________________________________


BASE_PARA_IVI<-select(DATA_BASE,PM,N_COMUN, N_CIENTIFICO,DAP ,AB ,VOL)
BASE_FOR_IVI<-BASE_PARA_IVI

TABLA_DE_IVI<-INDICE_VAR_IMP(BASE_PARA_IVI)

GUARDADO_FUN(RUTAS_ARCH[2],"TABLA_DE_IVI",TABLA_DE_IVI)

rm(BASE_FOR_IVI)

#___________________________________________________________________________________________

#TABLA DE INDICE DE VALOR DE COVRTURA 
#___________________________________________________________________________________________

INDICE_CV<-(TABLA_DE_IVI$ABUN_REL+TABLA_DE_IVI$DOM_REL)/2

TABLA_DE_VC<-TABLA_DE_IVI %>% select(N_CIENTIFICO,N_COMUN,ABUN_REL,DOM_REL)
TABLA_DE_VC<-cbind(TABLA_DE_VC,(INDICE_CV))

GUARDADO_FUN(RUTAS_ARCH[2],"tabla de valor decovertura",TABLA_DE_VC)

rm(INDICE_CV)
#___________________________________________________________________________________________

#GRAFICAS PARA EL ANALSIS DEL INVENTARIO
#___________________________________________________________________________________________

#GRAFICA DE TABLA DE RESUMEN DE PARCELAS SEGUN EL VOLUMEN Y AREA BASAL


RESUMEN_PARC<-cbind(RESUMEN_PARC,as.character(RESUMEN_PARC$PM))
colnames(RESUMEN_PARC)[ncol(RESUMEN_PARC)]<-"PARCELAS"

GRAFICA<-ggplot(data = RESUMEN_PARC)+
  geom_bar(mapping = aes(x=PARCELAS,y=VOL),stat = "identity")+coord_flip()+
  labs(title = "PARCELAS DE MUESTREO POR VOLUMEN DE MADERA EN PIE",x=" PARCELAS",y="VOLUMEN EN M3" )

png(file =paste(RUTAS_ARCH[2],"parcelas_vol",".png",sep = ""))
print(GRAFICA)
dev.off()

GRAFICA<-ggplot(data = RESUMEN_PARC)+
  geom_bar(mapping = aes(x=PARCELAS,y=AB),stat = "identity")+coord_flip()+
  labs(title = "PARCELAS DE MUESTREO POR AREA BASAL",x=" PARCELAS",y="AREA BASAL EN M2" )

png(file =paste(RUTAS_ARCH[2],"parcelas_ab",".png",sep = ""))
print(GRAFICA)
dev.off()


#5 ESPECIES CON MAYOR IVI 


AUXILIAR<-TABLA_DE_IVI[order(TABLA_DE_IVI$IVI_REAL,decreasing =T),]
AUXILIAR<-AUXILIAR[-c(1,7:nrow(AUXILIAR)),]

GRAFICA<-ggplot(data = AUXILIAR)+
  geom_bar(mapping = aes(x=N_CIENTIFICO,y=IVI_REAL),stat = "identity")+
  labs(title = "5 ESPECIES CON MAYOR IVI",y=("IVI"),x=("NOMBRE CIENTIFICO"))

png(file =paste(RUTAS_ARCH[2],"ivi",".png",sep = ""))
print(GRAFICA)
dev.off()


#ESPECIES COM MAYOR VOLUMEN Y AREA BASAL

LOL<-group_by(DATA_BASE,N_CIENTIFICO,N_COMUN) %>% summarise(N_ARB=n(),AB=sum(AB),VOL=sum(VOL))

AUXILIAR<-LOL[order(LOL$N_ARB,decreasing =T),]
AUXILIAR<-AUXILIAR[-c(6:nrow(DATA_BASE)),]

GRAFICA<-ggplot(data = AUXILIAR)+
  geom_bar(mapping = aes(x=N_CIENTIFICO,y=N_ARB),stat = "identity")+
  labs(title = "5 ESPECIES CON MAYOR NUMERO DE ARBOLES",y=("REPETICION"),x=("NOMBRE CIENTIFICO"))

png(file =paste(RUTAS_ARCH[2],"N_ARBOLES",".png",sep = ""))
print(GRAFICA)
dev.off()


AUXILIAR<-LOL[order(LOL$AB,decreasing =T),]
AUXILIAR<-AUXILIAR[-c(6:nrow(DATA_BASE)),]

GRAFICA<-ggplot(data = AUXILIAR)+
  geom_bar(mapping = aes(x=N_CIENTIFICO,y=N_ARB),stat = "identity")+
  labs(title = "5 ESPECIES CON MAYOR AREA BASAL",y=("AREA BASAL"),x=("NOMBRE CIENTIFICO"))

png(file =paste(RUTAS_ARCH[2],"AREA_BASAL",".png",sep = ""))
print(GRAFICA)
dev.off()


AUXILIAR<-LOL[order(LOL$VOL,decreasing =T),]
AUXILIAR<-AUXILIAR[-c(6:nrow(DATA_BASE)),]

GRAFICA<-ggplot(data = AUXILIAR)+
  geom_bar(mapping = aes(x=N_CIENTIFICO,y=N_ARB),stat = "identity")+
  labs(title = "5 ESPECIES CON MAYOR VOLUMEN",y=("VOLUMEN"),x=("NOMBRE CIENTIFICO"))

png(file =paste(RUTAS_ARCH[2],"VOLUMEN",".png",sep = ""))
print(GRAFICA)
dev.off()

