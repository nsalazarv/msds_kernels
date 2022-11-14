### Script pregunta 5 y 6
rm(list=ls())
pacman::p_load(tidyverse, openintro, ggplot2, kernlab, caret)
set.seed(23)

######### Pregunta 5 ######### 

## Revisando data

data(starbucks)
attach(starbucks)
summary(starbucks)

x_b = starbucks[2:3]
y_b = starbucks[7]

x_b = as.matrix(x_b)

y_b$type = as.numeric(y_b$type)
range = 1:77

for (i in range){
  if (y_b$type[i] != 1) {
    
    y_b$type[i] = 0
    
  } 
}

index1 = which(y_b == 1)
plot(unlist(x_b[,1]), unlist(x_b[,2]), xlab = 'Calories', ylab = 'Fat', main = 'Clasificación Bakery/otros')
points(unlist(x_b[index1,1]), unlist(x_b[index1,2]), pch = 19, col = "blue")

y_b = unlist(y_b)

## Estandarizando 

mux = apply(x_b,2,mean)
sdx = apply(x_b,2,sd)

x_est = x_b
x_est[,1] = (x_est[,1] - mux[1])/sdx[1]
x_est[,2] = (x_est[,2] - mux[2])/sdx[2]

x_est = as.matrix(x_est)

## Estableciendo funciones para predicción manual

Kfun = function(x, y, sigma)
{
  exp(-sigma*sum((x-y)^2))
}

predict_m = function(Xtest, Xtrain, Ksvm, sigma, threshold)
{
  ntest = dim(Xtest)[1]
  ntrain = dim(Xtrain)[1]
  
  Kpred = matrix(0, nrow = ntest, ncol = ntrain)
  
  for(a in 1:ntest)
  {
    for(s in 1:ntrain)
    {
      Kpred[a,s]=Kfun(Xtest[a,],Xtrain[s,],sigma)
      
    }
  }
  
  indices = Ksvm@alphaindex[[1]]
  coefs = Ksvm@coef[[1]]
  w_0 = -Ksvm@b
  
  yalpha = rep(0,ntrain)
  yalpha[indices] = coefs
  
  clase = as.vector(2*(Kpred%*%yalpha+w_0 > threshold))-1
  dists = as.vector(abs(Kpred%*%yalpha+w_0)) 
  
  out = list(clase = clase, dists = dists)
  return(out)
}

## Estableciendo thresholds de prueba

threshold = seq(-1,1,0.005)
len = length(threshold)

acc_vec = rep(0,len)
prec_vec = rep(0,len)
recall_vec = rep(0,len)
f1_vec = rep(0,len)
spec_vec = rep(0,len)

for(l in 1:len)
{
  if(l%%5==0) {print(paste('Iter:',l))}
  
  Ksvm = ksvm(y = as.factor(y_b), x = x_est, kernel = 'rbfdot', kpar = list(sigma = 0.1), scaled = FALSE, C = 1)
  pred = predict_m(x_est, x_est, Ksvm, 0.1, threshold[l])
  
  Table = table(pred$clase, as.factor(y_b))
  
  acc_vec[l] = (Table[1,1] + Table[2,2])/sum(Table)
  
  if(Table[2,1] + Table[2,2] == 0)prec_vec[l] = 0
  else prec_vec[l] = Table[2,2]/(Table[2,1] + Table[2,2])
  
  if(Table[1,2] + Table[2,2] == 0)recall_vec[l] = 0
  else recall_vec[l] = Table[2,2]/(Table[1,2] + Table[2,2])
  
  if(recall_vec[l] + prec_vec[l] == 0)f1_vec[l] = 0
  else f1_vec[l] = 2*(recall_vec[l]*prec_vec[l])/(recall_vec[l] + prec_vec[l])
  
  if(Table[1,1] + Table[2,1]==0)spec_vec[l]=0
  else spec_vec[l] = Table[1,1]/(Table[1,1] + Table[2,1])
}

par(mfrow=c(1,2))
plot(threshold, acc_vec, type='l', main = 'Accuracy')
plot(threshold, prec_vec, type='l', main = 'Precisión')

par(mfrow=c(1,2))
plot(threshold, recall_vec, type='l', main = 'Recall')
plot(threshold, f1_vec, type='l', main = 'F1 Score')

par(mfrow=c(1,2))
plot(threshold, acc_vec, type = 'l', main = 'Accuracy v/ Precision')
lines(threshold, prec_vec, col = 'blue')
plot(1-spec_vec, recall_vec)

## Podemos ver que el mejor modelo se encuentra en el un threshold aprox. = 0

plot(Ksvm, data = x_est)

pred2 = predict_m(x_est, x_est, Ksvm, 0.1, 0)
pred2

## Agregamos las predicciones, "distancias" absolutas calculadas, y una variable Flag.

data = data.frame(x_b, check.names = FALSE)
data$pred = pred2$clase
data$dist = pred2$dists
data$flag = FALSE

dists_sort = lapply(list(pred2$dists),sort)
dists_sort = unlist(dists_sort)
dists_sort

## Como tenemos 77 observaciones, el el 15% de las observaciones son aproximadamente 12.

max = dists_sort[12]

for(f in range){
  if(data$dist[f] <= max){
    
    data$flag[f] = TRUE
    
  }
  
}

par(mfrow=c(1,1))
index2 = which(data$flag == TRUE)
plot(unlist(data[,1]), unlist(data[,2]), xlab = 'Calories', ylab = 'Fat', main = 'Productos con inspección manual')
points(unlist(data[index2,1]), unlist(data[index2,2]), pch = 19, col = "red")

######### Pregunta 6 ######### 

data2 = starbucks
data2$type = as.numeric(data2$type)

for (q in range){
  if (data2$type[q] == 6) {

    data2$type[q] = 2

  }
}

for (w in range){
  if (data2$type[w] == 4 || data2$type[w] == 5 || data2$type[w] == 7) {

    data2$type[w] = 3

  }
}

x = data2[2:6]
y = unlist(data2[7])

## Estandarizando

mux = apply(x,2,mean)
sdx = apply(x,2,sd)

x_est = x
x_est[,1] = (x_est[,1] - mux[1])/sdx[1]
x_est[,2] = (x_est[,2] - mux[2])/sdx[2]
x_est[,3] = (x_est[,3] - mux[3])/sdx[3]
x_est[,4] = (x_est[,4] - mux[4])/sdx[4]
x_est[,5] = (x_est[,5] - mux[5])/sdx[5]

x_est = as.matrix(x_est)

## Tipo 1 vs todos

y1 = y

for(m in range){
  if (y1[m] != 1){
    y1[m] = 0
  }
}


Ksvm1 = ksvm(y = as.factor(y1), x = x_est, kernel = 'rbfdot', kpar = list(sigma = 1), scaled = FALSE, C = 1)
pred_1 = predict(Ksvm1, x_est)

## Tipo 2 vs todos

y2 = y

for(n in range){
  if (y2[n] != 2){
    y2[n] = 0
  }
}


Ksvm2 = ksvm(y = as.factor(y2), x = x_est, kernel = 'rbfdot', kpar = list(sigma = 1), scaled = FALSE, C = 1)
pred_2 = predict(Ksvm2, x_est)

## Tipo 3 vs todos

y3 = y

for(b in range){
  if (y3[b] != 3){
    y3[b] = 0
  }
}


Ksvm3 = ksvm(y = as.factor(y3), x = x_est, kernel = 'rbfdot', kpar = list(sigma = 1), scaled = FALSE, C = 1)
pred_3 = predict(Ksvm3, x_est)

## Armando modelo de ensamble

train_df = data.frame(pred_1, pred_2, pred_3, type = as.factor(y), check.names = FALSE)

modelStack = train(type ~ ., data = train_df, method = "rf")
combPred = predict(modelStack, train_df)

Table2 = table(combPred, y)

acc = (Table2[1,1] + Table2[2,2] + Table2[3,3])/sum(Table)
acc
              