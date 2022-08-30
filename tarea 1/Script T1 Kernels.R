
#### Script tarea 1 Kernels || Mateo Rojas, Nelson Salazar y Nicolás Valenzuela

library(ggplot2)
library(openintro)
library(caret)
library(reshape2)
library(tidyverse)

set.seed(6)

x<-cbind(seq(-2,2,length=700),seq(1,-1,length=700))
y<-cbind(seq(-4,4,length=700),seq(3,-3,length=700))

### Pregunta 1

# Entrega vector, norma con el valor del kernel.
x1<-c(1,2)
x2<-c(3,4)
Kernelp<-function(a,b,l,p,desv){
  lista<-c()
  lista2<-c()
  norma<-sqrt(sum((a-b)^2))
  resultado<-(desv^2)* 
    exp(-((2*(sin(pi*norma*(1/p)))^2)/(l^2)))*
    exp(-((norma)^2)/(2*(l^2)))
  lista<-rbind(lista,norma)
  lista2<-rbind(lista2,resultado)
  matriz<-cbind(lista,lista2)
  return(matriz)
}
# Entrega el valor del kernel.
Kernelp(x1,x2,1,0.5,1)[2]

# Graficamos para 1400 vectores.
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


### Pregunta 2

# ITEM 1
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


# ITEM 2

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


### Pregunta 3

set.seed(23)

## Parte 1
data(starbucks)
attach(starbucks)
summary(starbucks)
data <- select(starbucks, protein, calories)

# Separamos la DB en 5 conjuntos de datos.
index <- sample(1:5, 77, replace=TRUE) 
data['grupo'] <- index

# Creamos dataset de entrenamiento y prueba.
train <- data[which(data$grupo == 1 | data$grupo == 2 | data$grupo == 3 | data$grupo == 4),]
test <- data[which(data$grupo == 5),]

# Estimando los coeficientes de regresión con Mínimos Cuadrados
y <- train$protein
X <- cbind(rep(1,dim(train)[1]),train[["calories"]])

w <- solve(t(X)%*%X)%*%t(X)%*%y
w

# Estimando los coeficientes de regresión con la función lm()
modelo2 <- lm(protein~calories,data=train)
w2 <- modelo2[["coefficients"]]
w2

# Estandarizando la columna calories
processTrain <- train
processTest <- test

pre_proc_val <- preProcess(processTrain[,'calories'], method = c("center", "scale"))

processTrain[,'calories'] <- predict(pre_proc_val, processTrain[,'calories'])
processTest[,'calories'] <- predict(pre_proc_val, processTest[,'calories'])

# Repitiendo OLS con la columna calories estandarizada
y2 <- train$protein
X2 <- cbind(rep(1,dim(processTrain)[1]),processTrain[["calories"]])

w3 <- solve(t(X2)%*%X2)%*%t(X2)%*%y2
w3

# Graficando OLS sin y con estandarización
plot(train$calories, train$protein, main = 'OLS sin estandarización', xlab = 'Calories', ylab = 'Proteins')
abline(w)

plot(processTrain$calories, train$protein, main = 'OLS con estandarización', xlab = 'Calories', ylab = 'Proteins')
abline(w3)

# Predicción sin estandarización
X_pred <- cbind(rep(1,dim(test)[1]),test$calories)
y_pred <- X_pred%*%w
y_pred

plot(test$calories, test$protein, main = 'Predicción OLS sin estandarización', xlab = 'Calories', ylab = 'Proteins')
points(test$calories, y_pred, col = '2')
abline(w)

# Predicción con estandarización
X_pred2 <- cbind(rep(1,dim(processTest)[1]), processTest$calories)
y_pred2 <- X_pred2%*%w3
y_pred2

plot(processTest$calories, test$protein, main = 'Predicción OLS con estandarización', xlab = 'Calories', ylab = 'Proteins')
points(processTest$calories, y_pred2, col = '2')
abline(w3)

# ECMP OLS sin estandarización 
N = dim(test)[1]
ecmp <- 1/N*sum((test$protein - y_pred)^2)
ecmp

# ECMP OLS con estandarización 
ecmp2 <- 1/N*sum((test$protein - y_pred2)^2)
ecmp2

# Item anterior k=5
a<- c(1,2,3,4,5)
b<- c(2,3,4,5,1)
c<- c(3,4,5,1,2)
d<- c(4,5,1,2,3)
e<- c(5,1,2,3,4)
iterador <- data.frame(a, b, c, d, e)

error_cuadratico_medio <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Normal", "Estandarizado")
colnames(error_cuadratico_medio) <- x

par(mfrow=c(2,3))

for(i in 1:5){
  train <- data[which(data$grupo == iterador$a[i] | data$grupo == iterador$b[i] | data$grupo == iterador$c[i] | data$grupo == iterador$d[i]),]
  test <- data[which(data$grupo == iterador$e[i]),]
  
  y <- train$protein
  X <- cbind(rep(1,dim(train)[1]),train[["calories"]])
  w <- solve(t(X)%*%X)%*%t(X)%*%y
  
  modelo2 <- lm(protein~calories,data=train)
  w2 <- modelo2[["coefficients"]]
  
  processTrain <- train
  processTest <- test
  
  pre_proc_val <- preProcess(processTrain[,'calories'], method = c("center", "scale"))
  processTrain[,'calories'] <- predict(pre_proc_val, processTrain[,'calories'])
  processTest[,'calories'] <- predict(pre_proc_val, processTest[,'calories'])
  
  y2 <- train$protein
  X2 <- cbind(rep(1,dim(processTrain)[1]),processTrain[["calories"]])
  w3 <- solve(t(X2)%*%X2)%*%t(X2)%*%y2
  
  X_pred <- cbind(rep(1,dim(test)[1]),test$calories)
  y_pred <- X_pred%*%w
  X_pred2 <- cbind(rep(1,dim(processTest)[1]), processTest$calories)
  y_pred2 <- X_pred2%*%w3
  
  N = dim(test)[1]
  ecmp <- 1/N*sum((test$protein - y_pred)^2)
  ecmp2 <- 1/N*sum((test$protein - y_pred2)^2)
  error_cuadratico_medio[nrow(error_cuadratico_medio) + 1,] = c(ecmp, ecmp2)
  boxplot(error_cuadratico_medio)
  title(e[i])
}



## Parte 2
rm(list=ls())
data2 <- select(starbucks, protein, fat, carb, fiber, calories)
set.seed(100)
index = sample(1:nrow(data2),0.7*nrow(data2),replace=FALSE)
index=sort(index)
train = data2[index,] # Creamos datos de entrenamiento 
test = data2[-index,] # Creamos datos de evaluación
dim(train)
dim(test)
ptrain=train
ptest=test
cols = c( 'fat', 'carb', 'fiber','calories')
pre_proc_val <- preProcess(ptrain[,cols], method = c('center','scale'))
ptrain[,cols] = predict(pre_proc_val, ptrain[,cols])
ptest[,cols] = predict(pre_proc_val, ptest[,cols])


modelo_parte2=lm(protein~fat+carb+fiber+calories,data=data2)#Si entrenamos el modelo con la totalidad de la data obtenemos los resultados referenciados mediante conocimiento experto
summary(modelo_parte2) 


modelo_parte2=lm(protein~fat+carb+fiber+calories,data=data2)
summary(modelo_parte2)

#Podemos utilziar la data considerada por el conocimiento experto
#Restringimos w2 a ser -0.8
#Recreamos el modelo con data de entrenamieto
modelo_parte2=lm(protein+0.8*carb~fat+fiber+calories,data=ptrain)
summary(modelo_parte2) 


w=modelo_parte2[["coefficients"]]
X=cbind(rep(1,dim(ptrain)[1]),ptrain[["fat"]],ptrain[["fiber"]],ptrain[["calories"]])
y=ptrain[["protein"]]+0.8*ptrain[["carb"]]
w_manual=solve(t(X)%*%X)%*%t(X)%*%y

hat_y=modelo_parte2[["fitted.values"]]
hat_y_manual=X%*%w_manual
n=length(y)
plot(1:n,y,type="l")
points(1:n,hat_y,col="2")
points(1:n,hat_y_manual,col="3")

y_pred=predict.lm(modelo_parte2,newdata=ptest)
aux=summary(modelo_parte2)
aux[["adj.r.squared"]]
aux[["r.squared"]]
N=dim(ptest)[1]
1/N*sum((ptest[["protein"]]-predict.lm(modelo_parte2,newdata=ptest))^2)

#Para el cross validation utilizamos el control de la forma
library(caret)
ctrl <- trainControl(method = "cv", number = 5)
modelcrossval <- train(protein+0.8*carb~fat+fiber+calories,data=ptrain, method = "lm", trControl = ctrl) #Entrenamos el modelo considerando el control
print(modelcrossval)#Vemos el resumen del modelo con control k-fold CV
