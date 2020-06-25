rm(list=ls())
if (!require('magrittr')) install.packages('magrittr')
if (!require('kableExtra')) install.packages('kableExtra')

numero_corridas<-c(10, 100, 1000, 10000)
tiempo_total<-vector(mode="numeric",length=length(numero_corridas))
tiempo_promedio<-vector(mode="numeric",length=length(numero_corridas))
varianza_estimada<-vector(mode="numeric",length=length(numero_corridas))
desvio_estandar_estimado<-vector(mode="numeric",length=length(numero_corridas))  

#probar con 10, 100, 1000 y 10000 corridas
for (j in 1:length(numero_corridas)) {
  set.seed(10)
  tiempo_inicio <- Sys.time() 
  
  
  
  n<-numero_corridas[j]
  
  
  t1<-vector(mode="numeric",length=n)
  t2<-vector(mode="numeric",length=n)
  t3<-vector(mode="numeric",length=n)
  t4<-vector(mode="numeric",length=n)
  t5<-vector(mode="numeric",length=n)
  t6<-vector(mode="numeric",length=n)
  t7<-vector(mode="numeric",length=n)
  t8<-vector(mode="numeric",length=n)
  t9<-vector(mode="numeric",length=n)
  t10<-vector(mode="numeric",length=n)
  
  
  t1<-runif(n,40,56) %>% round(4) %>% as.vector()
  t2<-t1+runif(n,24,32) %>% round(4) %>% as.vector()
  t3<-t1+runif(n,20,40) %>% round(4) %>% as.vector()
  
  for(i in 1:n){
    t4[i]<-max(t2[i], t3[i])
  }
  t5<-t4
  t4<-t4+runif(n,16,48) %>% round(4) %>% as.vector()
  t5<-t5+runif(n,10,30) %>% round(4) %>% as.vector()
  t6<-t3+runif(n,15,30) %>% round(4) %>% as.vector()
  t7<-t3+runif(n,20,25) %>% round(4) %>% as.vector()
  
  for(i in 1:n){
    t8[i]<-max(t4[i], t5[i], t6[i], t7[i])
  }
  t8<-t8+runif(n,30,50) %>% round(4) %>% as.vector()
  t9<-t5+runif(n,40, 60) %>% round(4) %>% as.vector()
  
  for(i in 1:n){
    t10[i]<-max(t7[i], t8[i], t9[i])
  }
  
  t10<-t10+runif(n,8, 16) %>% round(4) %>% as.vector()
  
  s_n<-vector(mode="numeric",length=n)
  
  #suma acumulada
  for(i in 2:n){
    s_n[1]<-t10[1]  
    s_n[i]<-s_n[i-1]+t10[i]
  }
  
  tiempo_promedio[j]<-s_n[n]/n
  
  t_cuad<-vector(mode="numeric",length=n)
  
  for(i in 1:n){
    t_cuad[i]<-(t10[i])^2  
  }
  
  s_cuad<-vector(mode="numeric",length=n)
  for(i in 2:n){
    s_cuad[1]<-t_cuad[1]  
    s_cuad[i]<-s_cuad[i-1]+t_cuad[i]
  }
  
  
  varianza_estimada[j]<-(s_cuad[n]/((n-1)*n))-(tiempo_promedio[j]^0.5)/(n-1)
  desvio_estandar_estimado[j]<-varianza_estimada[j]^0.5
  
  tiempo_finalizacion <- Sys.time()
  
  tiempo_total[j]<-tiempo_finalizacion-tiempo_inicio
  tiempo_total
  
}

tabla<-rbind(tiempo_promedio,
             varianza_estimada, 
             desvio_estandar_estimado,  
             tiempo_total)

rownames(tabla)<-c("tiempo promedio" , "varianza estimada", 
                   "desvio estandar estimado", "tiempo corrida")
colnames(tabla)<-c("10", "100", "1000", "10000")
options(scipen = 9999999)
tabla %>% as.table()


