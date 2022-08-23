library(ggplot2)
set.seed(6)

x<-cbind(seq(-2,2,length=700),seq(1,-1,length=700))
y<-cbind(seq(-4,4,length=700),seq(3,-3,length=700))
####  Ejercicio 1  ####
#Entrega vector, norma con el valor del kernel.
x1<-c(1,2)
x2<-c(3,4)
Kernelp<-function(a,b,l,p,desv){
  lista<-c()
  lista2<-c()
  norma<-sqrt(sum((a-b)^2))
  resultado<-(desv^2)*exp(-((2*(sin(pi*norma*(1/p)))^2)/(l^2)))*exp(-((norma)^2)/(2*(l^2)))
  lista<-rbind(lista,norma)
  lista2<-rbind(lista2,resultado)
  matriz<-cbind(lista,lista2)
  return(matriz)
}
#Entrega el valor del kernel.
Kernelp(x1,x2,1,0.5,1)[2]

#graficamos para 1400 vectores.
graf<-c()
for (i in 1:700){
    valor<-Kernelp(x[i,],y[i,],1,0.5,1)
    graf<-rbind(graf,valor)
}
plot(graf[,1],graf[,2],type="l",col="red",
     xlab="Valor Norma",ylab="Valor Kernel",
     main="Locally Kernel σ = 1, p = 0.5, l=1")

graf2<-c()
for (i in 1:700){
  valor2<-Kernelp(x[i,],y[i,],10,0.5,1)
  graf2<-rbind(graf2,valor2)
}

plot(graf2[,1],graf2[,2],type="l",col="blue",
     xlab="Valor Norma",ylab="Valor Kernel",
     main="Locally Kernel σ = 1, p = 0.5, l=10")

#importar paquete reshape
#Generamos el grafico de ambos kernel para comparar.
df<-data.frame(cbind(graf,graf2[,2]))
colnames(df)<-c("X","l=1","l=10")
df <- melt(df, id.vars = "X")

ggplot<-ggplot(df, aes(x = X, y = value,group=variable, color = variable))+
  geom_line(size=0.5)+ggtitle("Locally Kernel σ = 1, p = 0.5// l=1 vs l=10")
ggplot<-ggplot+xlab("Valor Norma")+ylab("Valor Kernel")+theme(legend.title=element_blank())
ggplot

##Grafico de la función##
#Buscamos ver el compartamiento para un set ordenado de datos respecto a su norma.
#definidmos kernelp_graf con el objetivo de hacer el grafico respecto a una norma arbitraria.
Kernelp_graf<-function(l,p,desv){
  norma<-seq(0,3,length=100)
  resultado<-(desv^2)*exp(-((2*(sin(pi*norma*(1/p)))^2)/(l^2)))*exp(-((norma)^2)/(2*(l^2)))
  matriz<-cbind(norma,resultado)
  return(matriz)
}

graf3<-Kernelp_graf(1,0.5,1)
graf4<-Kernelp_graf(10,0.5,1)
graf5<-Kernelp_graf(1.5,0.5,1)
graf6<-Kernelp_graf(4,0.5,1)
plot(graf3[,1],graf3[,2],type="l",col="red")
plot(graf4[,1],graf4[,2],type="l",col="blue")

#Generamos el grafico de ambos kernel para comparar.
df2<-data.frame(cbind(graf3,graf5[,2],graf6[,2],graf4[,2]))
colnames(df2)<-c("X","l=1","l=1.5","l=4","l=10")
df2 <- melt(df2, id.vars = "X")
ggplot2<-ggplot(df2, aes(x = X, y = value,group=variable, color = variable))+
  geom_line(size=0.5) +ggtitle("Locally Kernel σ = 1, p = 0.5, para distintos l")
ggplot2<-ggplot2+theme(legend.title=element_blank())+
  xlab("Valor Norma")+ylab("Valor Kernel")
ggplot2


## Ejercicio 2##
#ITEM 1#
funcion=function(a,b)
{
  n1=nrow(a)
  n2=ncol(a)
  n3=nrow(b)
  n4=ncol(b)
  
  if(n1==n2 & n2==n3 & n4==50){
    c=matrix(rep(0, nrow(a)*ncol(b)), nrow = nrow(a))
    for(i in 1:nrow(a)){
      for(j in 1:ncol(b)){
        for(k in 1:nrow(b)){
          c[i,j] <- c[i,j] + a[i,k]*b[k, j]
        }
      }
    }
    return(c)
  }
  else{
    return("No se cumplen las condiciones")
  }
  
}
a=cbind(c(1,2,3),c(0,1,0))
b=cbind(c(1,4),c(2,5),c(3,6),c(0,7))
algo<-system.time(funcion(a,b))[1]
#algo<-unname(algo)
#algo
#algo<-cbind(algo)
#a%*%b

#ITEM 2#

set.seed(6)
tiempo_f=list()
tiempo_r=list()
eje1<-list()

for(i in 0:100){
  n=100*i+10
  if(n<10000){
    matrizA<-matrix(sample(1:4,n*n,replace=TRUE),ncol=n)
    matrizB<-matrix(sample(1:4,n*50,replace=TRUE),ncol=50)
    
    aux<-n
    eje1<-c(eje1,aux)
    
    tiempo1<-unname(system.time(funcion(matrizA,matrizB))[3])
    tiempo_f<-c(tiempo_f,tiempo1)
    
    tiempo2<-unname(system.time(matrizA%*%matrizB)[3])
    tiempo_r<-c(tiempo_r,tiempo2)
  }
  else{
    n=10000
    matrizA<-matrix(sample(1:4,n*n,replace=TRUE),ncol=n)
    matrizB<-matrix(sample(1:4,n*50,replace=TRUE),ncol=50)
    
    aux<-n
    eje1<-c(eje1,aux)
    
    tiempo1<-unname(system.time(funcion(matrizA,matrizB))[3])
    tiempo_f<-c(tiempo_f,tiempo1)
    
    tiempo2<-unname(system.time(matrizA%*%matrizB)[3])
    tiempo_r<-c(tiempo_r,tiempo2)
    break
  }

}
print("listo")


plot(eje1,tiempo_f,type="l", main="Tiempo de ejecución función",xlab="Tamaño de n", 
     ylab="Tiempo en segundos",cex.axis=0.5,las=1,ylim=c(0,1000),col="red",lwd=2)

plot(eje1,tiempo_r,type="l", main="Tiempo de ejecución nativa",xlab="Tamaño de n", 
     ylab="Tiempo en segundos",cex.axis=0.5,las=1,ylim=c(0,5),col="blue", lwd=2)

plot(eje1,tiempo_f,type="l", main="Tiempo de ejecución función v/s nativa",
     xlab="Tamaño de n", 
     ylab="Tiempo en segundos",lwd=2,col="red"
     ,xlim=c(0,10100),ylim=c(0,1000),las=1,cex.axis=0.5)
lines(eje1,tiempo_r,lwd=2,col="blue")
legend("topleft",c("Función","Nativa"),col=c("red","blue")
       ,lwd=c(2,2),cex=0.5)