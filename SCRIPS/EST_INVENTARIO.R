#ANALSIS DE ESTADISTICA DESCRIPTIVA
EST_DESCR<-function(DATOS_P,N_SIGNIFICANCIA){
  DATOS_P<-data.frame(DATOS_P)
  colnames(DATOS_P)<-"V1"
  
  ASIMETRIA<-function(x){
    Q<-quantile(x,c(.25,.50,.75))
    return (((Q[3] - Q[2]) - (Q[2] - Q[1])) / (Q[3] - Q[1]))
  }
  MODA<-function(x){
    return(as.numeric(names(which.max(table(x)))))
  }
  
  PROMEDIO<-mean(DATOS_P$V1)
  ERROR_TIPICO<-std.error(DATOS_P$V1)
  MEDIANA<-median(DATOS_P$V1)
  MODA<-MODA(DATOS_P$V1)
  DESV_EST<-sd(DATOS_P$V1)
  VARIANZA<-DESV_EST^2
  CURTOLISIS<-sum((DATOS_P-mean(DATOS_P$V1))^4)/(length(DATOS_P$V1)*(sd(DATOS_P$V1)^4))
  ASIMETRIA<-ASIMETRIA(DATOS_P$V1)
  MAXIMO<-max(DATOS_P$V1)
  MINIMO<-min(DATOS_P$V1)
  RANGO<-MAXIMO-MINIMO
  SUMA<-sum(DATOS_P$V1)
  CV<-DESV_EST/PROMEDIO
  
  MARGEN_ERR<-qt(N_SIGNIFICANCIA,length(DATOS_P$V1))*ERROR_TIPICO
  INTER_POSIT<-PROMEDIO+MARGEN_ERR
  INTER_NEGAT<-PROMEDIO-MARGEN_ERR
  
  TABLA_B<-data.frame(rbind(PROMEDIO,ERROR_TIPICO,MEDIANA,MODA,DESV_EST,VARIANZA,CURTOLISIS,ASIMETRIA,RANGO,MAXIMO,MINIMO,SUMA,
                            CV,MARGEN_ERR,INTER_POSIT,INTER_NEGAT))
  TABLA_B<-data.frame(row.names(TABLA_B),TABLA_B)
  
  colnames(TABLA_B)<-c("ESTADISTICOS","VALORES")
  
  rownames(TABLA_B)<-c(1:length(TABLA_B$ESTADISTICOS))
  
  return(TABLA_B)
}