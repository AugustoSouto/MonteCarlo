rm(list = ls())
#parametros#
r<-0.4
n<-10^6

#sorteo de valores en el hipercubo#
x<-runif(n, 0, 1) %>% as.matrix()
y<-runif(n, 0, 1) %>% as.matrix()

coor<-cbind(x, y)

#centro#
c_x<-rep(0.5,  n)
c_y<-rep(0.5,  n)
cent<-cbind(c_x, c_y)

valor<-vector("numeric", length=n)
#valores que toma la función en los puntos sorteados dentro de la region#
for (i in 1:n) {
  if(sum((coor[i,]-cent[i,])^2)<=r^2){
    valor[i]=(200*x[i]*y[i])/max(x[i],y[i])
  }else{
    valor[i]=0}
}

s<-vector("numeric", length=n)
t<-vector("numeric", length=n)

t[1]=0
s[1]=valor[1]

for (i in 2:n) {
    t[i]<-t[i-1]+(1-(1/i))*(valor[i] -( s[i-1] / (i-1)) )^2 
    s[i]<-s[i-1]+valor[i]
}


int<-s[n]/n
var_int<-t[n]/(n-1)
var_est<-var_int/n

z2<-qnorm(0.975)^2
epsilon<-sqrt(n/(z2*var_int))



n_es<-(z2*var_int)*10^2
n_es

inf<-int-qnorm(0.975)*(var_int/n_es)^(1/2)
sup<-int+qnorm(0.975)*(var_int/n_es)^(1/2)

