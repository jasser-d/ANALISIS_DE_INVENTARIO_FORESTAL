#-------------------------------------------------

#FUNCION DE LCASIFICAION DIAMETRICA

#----------------------------------------------------------------------------------
#para usar esta funcion se debe de introducir una tabla con las sigueintes variables
#nombre comun, nombre cientifico ,DIAMETRO (cm) , area basal y volumen
#debe tener los siguientes nombres de columnas:
#  N_COMUN, N_CIENTIFICO ,DAP ,AB ,VOL
#--------------------------------------------------------------------------------
CLASS_DAP_30<-function(BASE_INFORM){

  DAP_CLASS<-cut(BASE_INFORM$DAP,breaks = c(0,40,50,60,70,80,90,100,Inf),
                 labels=c("30","40","50","60","70","80","90","100"),right = F)
  
  BASE_INFORM<-cbind(BASE_INFORM,DAP_CLASS)
  TABLA_DIAMETRICA<-distinct(BASE_INFORM,N_COMUN,N_CIENTIFICO)
  
  BASE_INFORM<-group_by(BASE_INFORM,DAP_CLASS,N_CIENTIFICO)
  BASE_INFORM<-summarise(BASE_INFORM,N=n(),AB=sum(AB),VOL=sum(VOL))
  
  SUM_TOTAL<-group_by(BASE_INFORM,N_CIENTIFICO) %>% summarise(N_T=sum(N),AB_T=sum(AB),VOL_T=sum(VOL))
  SUMA_TOTAL<-summarise(SUM_TOTAL,x=sum(N_T),y=sum(AB_T),z=sum(VOL_T))
  
  TABLA_DIAMETRICA<-rbind(TABLA_DIAMETRICA,"TOTAL","PORCENTAJE")
  
  ITER<-20
  
  for (i in 1:8) {
    
    ITER<-ITER+10
    
    D_AUX<-filter(BASE_INFORM,DAP_CLASS==ITER)
    D_AUX<-ungroup(D_AUX)
    D_AUX<-D_AUX %>% select(-one_of("DAP_CLASS"))
    
    TEXT_N<-paste("N",ITER,sep = "_")
    TEXT_AB<-paste("AB",ITER,sep = "_")
    TEXT_VOL<-paste("VOL",ITER,sep = "_")
    
    SUMAS_VAR<-summarise(D_AUX,N=sum(N),AB=sum(AB),VOL=sum(VOL)) %>%cbind("TOTAL")
    PORC_CD<-data.frame()
    
    for (i in 1:3) {
      PORC_CD[1,i]<-(SUMAS_VAR[i]/SUMA_TOTAL[i])*100
    }
    
    PORC_CD<-cbind(PORC_CD,"PORCENTAJE")
    
    colnames(PORC_CD)<-c(TEXT_N,TEXT_AB,TEXT_VOL,"N_CIENTIFICO")
    colnames(SUMAS_VAR)<-c(TEXT_N,TEXT_AB,TEXT_VOL,"N_CIENTIFICO")
    colnames(D_AUX)<-c("N_CIENTIFICO",TEXT_N,TEXT_AB,TEXT_VOL)
    
    D_AUX<-rbind(D_AUX,SUMAS_VAR,PORC_CD)
    
    TABLA_DIAMETRICA<-merge(x=TABLA_DIAMETRICA,y=D_AUX,all.x = T)
    
  }
  SUMA_TOTAL<-summarise(SUM_TOTAL,sum(N_T),sum(AB_T),sum(VOL_T)) %>% cbind("TOTAL")
  colnames(SUMA_TOTAL)<-c("N_T","AB_T","VOL_T","N_CIENTIFICO")
  
  T_PORC<-data.frame(cbind(SUMA_TOTAL$N_T/SUMA_TOTAL$N_T,SUMA_TOTAL$AB_T/SUMA_TOTAL$AB_T,
                           SUMA_TOTAL$VOL_T/SUMA_TOTAL$VOL_T)*100) %>% cbind("PORCENTAJE")
  colnames(T_PORC)<-c("N_T","AB_T","VOL_T","N_CIENTIFICO")
  
  SUM_TOTAL<-rbind(SUM_TOTAL,SUMA_TOTAL,T_PORC)
  TABLA_DIAMETRICA<-merge(x=TABLA_DIAMETRICA,y=SUM_TOTAL,all.x = T)
  return(TABLA_DIAMETRICA)
}
