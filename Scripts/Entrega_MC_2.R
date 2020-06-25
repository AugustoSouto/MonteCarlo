####PARTE A DEL EJERCICIO###
rm(list=ls())
if (!require('magrittr')) install.packages('magrittr')
if (!require('kableExtra')) install.packages('kableExtra')
#PARTE A DEL EJERCICIO 3.1
numero_corridas<-c(10^4,10^6)



volumen_estimado <-vector("numeric", length(numero_corridas))
var_estimada  <-vector("numeric", length(numero_corridas))
desvio        <-vector("numeric", length(numero_corridas))
inicio        <-vector("numeric", length(numero_corridas))
fin           <-vector("numeric", length(numero_corridas))
tiempo_corrida<-vector("numeric", length(numero_corridas))

for (k in 1:length(numero_corridas)) {
  
  inicio[k]     <-Sys.time()  
  
  n<-numero_corridas[k]
  
  set.seed(10)
  
  puntos<-replicate(n, runif(6,0,1), simplify=T)  
  
  centro<-c(0.4,0.45,0.55, 0.6, 0.5, 0.4) %>% as.vector()
  radio<-0.4
  
  S<-vector("numeric", n)
  
  #FUNCION INDICATRIZ#
  #DEVUELVE 1 SI EL PUNTO PERTENECE A LA REGION DE INTERES, 0 SI NO#
  
  for(i in 1:n){
    
    if( sum((puntos[,i]-centro)^2)<=radio^2  &&
        5*puntos[1,i]+10*puntos[2,i]<=4 && 
        puntos[3,i]+puntos[4,i]<=1 &&
        2*puntos[3,i]-3*puntos[2,i]+puntos[5,i]+puntos[6,i]>=0
    )
    {S[i]=1}else{S[i]=0}
  }  
  
  s_ac<-vector("numeric",n)
  
  for(i in 2:n){
    s_ac[1]<-S[1]  
    s_ac[i]<-s_ac[i-1]+S[i]
  }
  
  volumen_estimado[k]<-s_ac[n]/n
  
  var_estimada[k]<-(volumen_estimado[k]*(1-volumen_estimado[k]))/n
  
  desvio[k]<-var_estimada[k]^(1/2)
  
  
  fin[k]<-Sys.time()
  
  tiempo_corrida[k]<-fin[k]-inicio[k]
  
}


tabla_A<-rbind(volumen_estimado,
               var_estimada, 
               desvio,  
               tiempo_corrida)

colnames(tabla_A)<-c("10^4", "10^6")

rownames(tabla_A)<-c("volumen" , "varianza", 
                     "desvio", "tiempo")

####PARTE B DEL EJERCICIO###

volumen_verdadero<-(pi^3)*(radio^6)/6
numero_corridas<-c(10^4,10^6)

volumen_est_hipesf<-vector("numeric", length(numero_corridas))
var_est_hipesf    <-vector("numeric", length(numero_corridas))
desvio_hipesf     <-vector("numeric", length(numero_corridas))
inicio            <-vector("numeric", length(numero_corridas))
fin               <-vector("numeric", length(numero_corridas))
tiempo_corrida    <-vector("numeric", length(numero_corridas))
diferencia        <-vector("numeric", length(numero_corridas))
for (k in 1:length(numero_corridas)) {
  
  inicio[k]<-Sys.time()  
  
  n<-numero_corridas[k]
  
  set.seed(10)
  
  puntos<-replicate(n, runif(6,0,1), simplify=T)  
  
  centro<-c(0.4,0.45,0.55, 0.6, 0.5, 0.4) %>% as.vector()
  radio<-0.4
  
  S<-vector("numeric", n)
  
  #FUNCION INDICATRIZ#
  #DEVUELVE 1 SI EL PUNTO PERTENECE A LA REGION DE INTERES, 0 SI NO#
  
  for(i in 1:n){
    if( sum((puntos[,i]-centro)^2)<=radio^2){S[i]=1}else{S[i]=0}
  }  
  
  
  s_ac<-vector("numeric",n)
  
  for(i in 2:n){
    s_ac[1]<-S[1]  
    s_ac[i]<-s_ac[i-1]+S[i]
  }
  
  volumen_est_hipesf[k]<-s_ac[n]/n
  
  var_est_hipesf[k]<-(volumen_est_hipesf[k]*(1-volumen_est_hipesf[k]))/n
  
  desvio_hipesf[k]<-var_est_hipesf[k]^(1/2)
  
  
  fin[k]<-Sys.time()
  
  tiempo_corrida[k]<-fin[k]-inicio[k]
  
  diferencia[k]<-abs(volumen_est_hipesf[k]-volumen_verdadero)
}


formatC(volumen_est_hipesf, format = "e", digits = 2)
formatC(var_est_hipesf, format = "e", digits = 2)
formatC(desvio_hipesf, format = "e", digits = 2)
formatC(tiempo_corrida, format = "e", digits = 2)

tabla_B<-rbind(volumen_est_hipesf,
               var_est_hipesf, 
               desvio_hipesf,  
               tiempo_corrida)


colnames(tabla_B)<-c("10^4", "10^6")

rownames(tabla_B)<-c("volumen" , "varianza", 
                     "desvio", "tiempo")

