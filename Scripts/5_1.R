


####################################################################################
#                                      5.1                                         #
####################################################################################

#############################     A   ##############################################
rm(list=ls())
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

print(tabla_A)
###################         B        #################################################

#DADO EPSILON 5.10^-4 Y DELTA IGUAL A 0.05
n<-(log(40)*2)/(10^(-6))

print(n)


###################       c          #################################################

#########CALCULO DE INTERVALOS DE CONFIANZA SEGUN CRITERIO DE CHEBYSHEV################

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
  
 z<- s_ac[n]
 
 beta<-(0.05^-0.5)

w_1<-(z+(beta^2)/2 - beta*( (beta^2)/4 + z*(n-z)/n )^0.5) / (n+(beta^2))
w_2<-((z+(beta^2)/2) +beta*((beta^2)/4+z*(n-z)/n)^0.5)/ (n+(beta^2))

print(c(w_1, w_2)) #intervalo de confianza, Chebyshev#

print(z/n) #estimacion puntual#


########CALCULO DE INTERVALOS DE CONFIANZA SEGUN CRITERIO AGRESTI COULL################

ze<-z/n
nn<-n+qnorm(0.975)^2

ag_1<-ze-qnorm(0.975)*(ze*(1-ze)/(n+qnorm(0.975)^2)) 
ag_2<-ze+qnorm(0.975)*(ze*(1-ze)/(n+qnorm(0.975)^2))

print(c(ag_1, ag_1)) #intervalo de confianza, Agresti-Coull#