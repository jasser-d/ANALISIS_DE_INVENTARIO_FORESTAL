
INDICE_VAR_IMP<-function(BASE_FOR_IVI){

  BASE_FOR_IVI<-ungroup(BASE_FOR_IVI)
  
  TABLA_IVI<-distinct(BASE_FOR_IVI,N_CIENTIFICO,N_COMUN)
  
  FRECU_BASE<-select(TABLA_IVI,N_CIENTIFICO)
  
  TABLA_IVI<-rbind(TABLA_IVI,"TOTAL")
  
  BASE_FOR_IVI<-group_by(BASE_FOR_IVI,PM,N_CIENTIFICO,N_COMUN)
  
  BASE_FOR_IVI<-summarise(BASE_FOR_IVI,N=n(),AB=sum(AB),VOL=sum(VOL))
  
  DOM_ABS<-group_by(BASE_FOR_IVI,N_CIENTIFICO) %>%  summarise(DOM_ABS=sum(AB))
  TOTAL_DOM<-sum(DOM_ABS$DOM_ABS)
  DOM_REL<-(DOM_ABS$DOM_ABS/TOTAL_DOM)*100 %>% as.numeric()
  DOM_REL<-data.frame(N_CIENTIFICO=DOM_ABS$N_CIENTIFICO,DOM_REL=DOM_REL)
  
  ABUN_ABS<-group_by(BASE_FOR_IVI,N_CIENTIFICO) %>%  summarise(ABUN_ABS=sum(N))
  TOTAL_ABUN<-sum(ABUN_ABS$ABUN_ABS)
  ABUN_REL<-(ABUN_ABS$ABUN_ABS/TOTAL_ABUN)*100 %>% as.numeric()
  ABUN_REL<-data.frame(N_CIENTIFICO=ABUN_ABS$N_CIENTIFICO,ABUN_REL=ABUN_REL)
  
  FREC_ABS<-group_by(BASE_FOR_IVI,N_CIENTIFICO) %>%  summarise(FREC_ABS=n())
  TOTAL_FRE<-sum(FREC_ABS$FREC_ABS)
  FREC_REL<-(FREC_ABS$FREC_ABS/TOTAL_FRE)*100
  FREC_REL<-data.frame(N_CIENTIFICO=FREC_ABS$N_CIENTIFICO,FREC_REL=FREC_REL)
  
  BASE_IVI<-merge(x=FREC_ABS,y=ABUN_ABS,all.x = T)
  BASE_IVI<-merge(x=BASE_IVI,y=DOM_ABS,all.x = T)
  BASE_IVI<-merge(x=BASE_IVI,y=FREC_REL,all.x = T)
  BASE_IVI<-merge(x=BASE_IVI,y=ABUN_REL,all.x = T)
  BASE_IVI<-merge(x=BASE_IVI,y=DOM_REL,all.x = T)

  IVI300<-as.numeric(BASE_IVI$FREC_REL)+as.numeric(BASE_IVI$ABUN_REL)+as.numeric(BASE_IVI$DOM_REL)
  
  IVI_REAL<-IVI300/3
  
  BASE_IVI<-cbind(BASE_IVI,IVI300,IVI_REAL)
  
  TOTALES<-summarise(BASE_IVI,FREC_ABS=sum(FREC_ABS),ABUN_ABS=sum(ABUN_ABS),DOM_ABS=sum(DOM_ABS),FREC_REL=sum(as.numeric(FREC_REL))
                     ,ABUN_REL=sum(as.numeric(ABUN_REL)),DOM_REL=sum(as.numeric(DOM_REL)),IVI300=sum(as.numeric(IVI300)),
                     IVI_REAL=sum(as.numeric(IVI_REAL)))
  
  TOTALES<-cbind("TOTAL",TOTALES)
  colnames(TOTALES)[1]<-"N_CIENTIFICO"
  BASE_IVI<-rbind(BASE_IVI,TOTALES)
  
  BASE_FOR_IVI<-ungroup(BASE_FOR_IVI)
  
  for (i in 1:dim(distinct(BASE_FOR_IVI,PM))[1]) {
    AUXILIAR<-select(BASE_FOR_IVI,PM,N_CIENTIFICO,N)
    TEXT_COL<-paste("P",i,sep = "_")
    AUXILIAR<-filter(AUXILIAR,PM==i)
    AUXILIAR<- AUXILIAR%>% select(-one_of("PM"))
    TOTAL_IVI<-cbind("TOTAL",sum(AUXILIAR$N))
    colnames(AUXILIAR)[2]<-TEXT_COL
    colnames(TOTAL_IVI)<-c("N_CIENTIFICO",TEXT_COL)
    AUXILIAR<-rbind(AUXILIAR,TOTAL_IVI)
    TABLA_IVI<- merge(x=TABLA_IVI,y=AUXILIAR,all.x = T)
  }
  TABLA_IVI<-merge(x=TABLA_IVI,y=BASE_IVI,all.x = T)
  return(TABLA_IVI)
}
