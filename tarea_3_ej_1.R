library(ggplot2)
set.seed(6)

#Ejercicio 1  

###Creamos los kernel 1, 2 y 3 y los kernels construidos a partir de ellos:####
x1<-2
x2<-4
#Kernel 1
k1<-function(a,b,desv_v,c){
  #Preguntar que efectivamente sea el producto punto.
  resultado<-1+desv_v*(a-c)*(b-c)
  return(resultado)
}
k1(x1,x2,1,1)
#Kernel 2
k2<-function(a,b,l,p){
  norma<-abs(a-b)
  resultado<-exp(-((2*(sin(pi*norma*(1/p)))^2)/(l^2)))
  return(resultado)
}
k2(x1,x2,1,1)


#Periodico lineal.
per_lin<-function(a,b,desv_v,c,l,p){
  resultado<-k1(a,b,desv_v,c)*k2(a,b,l,p)
  return(resultado)
}

per_lin(x1,x2,1,1,1,1)

#Kernel 3.
k3<-function(a,b,s){
  resultado<-exp(-(((a-b)^2)/(2*(s^2))))
}

k3(x1,x2,1)

#Locally periodic
locally<-function(a,b,l,p,s){
  resultado<-k2(a,b,l,p)*k3(a,b,s)
  return(resultado)
}
locally(x1,x2,1,1,1)

#brownian
brownian<-function(a,b,desv){
  resultado<-(desv^2)*pmin(a,b)
  return(resultado)
}
brownian(x1,x2,1)

###Definimos las funciones RKHS asociado a cada uno de ellos.####
# Elegimos el parametro m
m=15
# Elegimos las constantes.
a=runif(m,-4,4)
# Elegimos los puntos zi.
z=runif(m,-2,2)


####1 Kernel Periodico: ####

#Creamos la función rkhs_periodic:
rkhs_periodic=function(x,desv_v,c,l,p)
{
  out=0
  for(i in 1:m)
  {
    out=out+a[i]*per_lin(x,z[i],desv_v,c,l,p)
  }
  out
}

#grilla entre -5 y 5
s=seq(-5,5,0.1)

out=c()
for(i in 1:length(s)){
  # rkhs_periodic(x,desv_v,c,l,p)
  out=c(out,rkhs_periodic(s[i],1,1,1,1))
}
df<-data.frame(cbind(s,out))
ggplot2<-ggplot(df,aes(x = s, y = out))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, p = 1, c=1, l=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2




#Parametros desv_v,c,l,p:
library(reshape2)


##Para el parametro c:####
graf_c<-c()
c_list=c(-5,-1,0,1,5)
for (j in c_list){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],1,j,1,1))
  }
  graf_c<-cbind(graf_c,out)
}
df2<-data.frame(cbind(s,graf_c))
colnames(df2)<-c("X","c=-5","c=-1","c=0","c=1","c=5")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, p = 1, l=1 para distintos c")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

#Para los c negativos.
graf_c<-c()
c_list_1=c(-1,-5,-10,-15,-25)
for (j in c_list_1){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],1,j,1,1))
  }
  graf_c<-cbind(graf_c,out)
}
df2<-data.frame(cbind(s,graf_c))
colnames(df2)<-c("X","c=-1","c=-5","c=-10","c=-15","c=-25")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, p = 1, l=1 para distintos c")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

#Para los c positivos.
graf_c<-c()
c_list_2=c(1,5,10,15,25)
for (j in c_list_2){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],1,j,1,1))
  }
  graf_c<-cbind(graf_c,out)
}
df2<-data.frame(cbind(s,graf_c))
colnames(df2)<-c("X","c=1","c=5","c=10","c=15","c=25")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, p = 1, l=1 para distintos c")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

##Para el parametro σ:####
graf_sigma<-c()
sigma_list=c(0.1,0.5,1,2,3)
for (j in sigma_list){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],j,0,1,1))
  }
  graf_sigma<-cbind(graf_sigma,out)
}
df2<-data.frame(cbind(s,graf_sigma))
colnames(df2)<-c("X","σ=0.1","σ=0.5","σ=1","σ=2","σ=3")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel c = 0, p = 1, l=1 para distintos σ")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2


##Para el parametro l:####
graf_l<-c()
l_list=c(0.3,1,2,5)
for (j in l_list){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],1,1,j,1))
  }
  graf_l<-cbind(graf_l,out)
}
df2<-data.frame(cbind(s,graf_l))
colnames(df2)<-c("X","l=0.3","l=1","l=2","l=5")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, p = 1, c=1 para distintos l")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

##Para el parametro p: ####
graf_p<-c()
p_list=c(0.25,0.5,1,5,10)
for (j in p_list){
  out=c()
  for(i in 1:length(s))
  {
    out=c(out,rkhs_periodic(s[i],1,1,1,j))
  }
  graf_p<-cbind(graf_p,out)
}
df2<-data.frame(cbind(s,graf_p))
colnames(df2)<-c("X","p=0.25","p=0.5","p=1","p=5","p=10")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Periodic linear kernel σ = 1, c = 1, l=1 para distintos p")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2



####2 Locally PERIODIC KERNEL ####
#Creamos la función rkhs_locally:
rkhs_locally=function(x,l,p,s)
{
  out=0
  for(i in 1:m)
  {
    out=out+a[i]*locally(x,z[i],l,p,s)
  }
  out
}

#grilla entre -5 y 5
grid<-seq(-5,5,0.1)
out<-c()
for(i in 1:length(grid)){
  # rkhs_locally(x,l,p,s)
  out=c(out,rkhs_locally(grid[i],1,1,1))
}

df<-data.frame(cbind(grid,out))
ggplot2<-ggplot(df,aes(x = grid, y = out))+
  geom_line() +ggtitle("Locally periodic kernel l=1, p=1, s=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2


#Parametros l,p,s:

#parametro l:
graf2_l<-c()
l_list2=c(0.3,0.5,0.7,1)
for (j in l_list2){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_locally(grid[i],j,1,1))
  }
  graf2_l<-cbind(graf2_l,out)
}
df2<-data.frame(cbind(grid,graf2_l))
colnames(df2)<-c("X","l=0.3","l=0.5","l=0.7","l=1")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Locally periodic kernel p=1, s=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

#parametro p:
graf2_p<-c()
p_list2=c(0.25,0.5,1,2,4)
for (j in p_list2){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_locally(grid[i],1,j,1))
  }
  graf2_p<-cbind(graf2_p,out)
}
df2<-data.frame(cbind(grid,graf2_p))
colnames(df2)<-c("X","p=0.25","p=0.5","p=1","p=2","p=4")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Locally periodic kernel l=1, s=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

graf2_p<-c()
p_list2=c(1,2,4)
for (j in p_list2){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_locally(grid[i],1,j,1))
  }
  graf2_p<-cbind(graf2_p,out)
}
df2<-data.frame(cbind(grid,graf2_p))
colnames(df2)<-c("X","p=1","p=2","p=4")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Locally periodic kernel l=1, s=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

graf2_p<-c()
p_list2=c(0.25,0.5,1)
for (j in p_list2){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_locally(grid[i],1,j,1))
  }
  graf2_p<-cbind(graf2_p,out)
}
df2<-data.frame(cbind(grid,graf2_p))
colnames(df2)<-c("X","p=0.25","p=0.5","p=1")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Locally periodic kernel l=1, s=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

#parametro s:
graf2_s<-c()
s_list2=c(0.1,0.5,1,2,5)
for (j in s_list2){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_locally(grid[i],1,1,j))
  }
  graf2_s<-cbind(graf2_s,out)
}
df2<-data.frame(cbind(grid,graf2_s))
colnames(df2)<-c("X","s=0.1","s=0.5","s=1","s=2","s=5")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Locally periodic kernel l=1, p=1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2


#### 3 Kernel Browniano ####
rkhs_brow=function(x,desv)
{
  out=0
  for(i in 1:m)
  {
    out=out+a[i]*brownian(x,z[i],desv)
  }
  out
}

#grilla entre -5 y 5
grid<-seq(-5,5,0.1)
out<-c()
for(i in 1:length(grid)){
  out=c(out,rkhs_brow(grid[i],1))
}

df<-data.frame(cbind(grid,out))
ggplot2<-ggplot(df,aes(x = grid, y = out))+
  geom_line() +ggtitle("kernel browniano σ =1")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

#rkhs_brow(x,desv)
graf3_desv<-c()
#parametro desv:
desv_list3=c(0.1,0.5,1,2)
for (j in desv_list3){
  out=c()
  for(i in 1:length(grid))
  {
    out=c(out,rkhs_brow(grid[i],j))
  }
  graf3_desv<-cbind(graf3_desv,out)
}
df2<-data.frame(cbind(grid,graf3_desv))
colnames(df2)<-c("X","σ=0.1","σ=0.5","σ=1","σ=2")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value, group=variable, color = variable))+
  geom_line() +ggtitle("Brownian Kernel")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("X")+ylab("Valor Kernel")
ggplot2

