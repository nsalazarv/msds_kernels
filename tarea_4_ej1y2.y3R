pacman::p_load(tidyverse, tidymodels)
library("kernlab")
library(openintro)
data(starbucks)
attach(starbucks)
summary(starbucks)

set.seed(57206)
data<-starbucks
data$type<- ifelse(data$type == 'bakery', 1, 0)
# Y=data$type
# data$type<-NULL
data$item<-NULL
# X=as.matrix(data)

# Prepare the data frame containing the cross validation partitions
folds <- vfold_cv(data, v = 5, strata = "type" )
folds


cv_data <- folds %>% 
  mutate(
    # Extract the train data frame for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate data frame for each split
    validate = map(splits, ~testing(.x))
  )

#3 es train
# cv_data[[3]][[1]][-1]
# #4 es test
# 
# Y=as.matrix(cv_data[[3]][[1]][6])
# X=as.matrix(cv_data[[3]][[1]][1:5])


###Pregunta 1. ####


##Sin Estandarizar##
resultados<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    X_train=as.matrix(cv_data[[3]][[i]][j])
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    
    #Testeo
    X_test=as.matrix(cv_data[[4]][[i]][j])
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    Precision=Table[2,2]/(Table[2,1]+Table[2,2])
    Recall=Table[2,2]/(Table[1,2]+Table[2,2])
    aux<-cbind(nombres[j],i,Accuracy,Precision,Recall)
    aux_2<-rbind(aux_2,aux)
  }
  resultados<-rbind(resultados,aux_2)
}


#resultados generales.
resultados<-data.frame(resultados)
colnames(resultados)<-c("Covariable", "Kfold", "Accuracy", "Precision", "Recall")
resultados_se<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados_se)
medias_se<-resultados_se %>%  group_by(Covariable) %>% summarise(Accuracy = mean(Accuracy), 
                                                   Precision = mean(Precision), 
                                                   Recall = mean(Recall))

#accuracy: proteina; precision carb y recall protein

##Estandarizado ##

resultados<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    X_train=cv_data[[3]][[i]][j]
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    
    #Testeo
    X_test=cv_data[[4]][[i]][j]
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    
    ## Estandarizando 
    
    mux = apply(X_train,2,mean)
    sdx = apply(X_train,2,sd)
    
    xtrain_est = X_train
    xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
    xtrain_est = as.matrix(xtrain_est)
    
    xtest_est = X_test
    xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
    
    
    xtest_est = as.matrix(xtest_est)
    X_train=xtrain_est
    X_test=xtest_est
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    Precision=Table[2,2]/(Table[2,1]+Table[2,2])
    Recall=Table[2,2]/(Table[1,2]+Table[2,2])
    aux<-cbind(nombres[j],i,Accuracy,Precision,Recall)
    aux_2<-rbind(aux_2,aux)
  }
  resultados<-rbind(resultados,aux_2)
}


#resultados generales.
resultados<-data.frame(resultados)
colnames(resultados)<-c("Covariable", "Kfold", "Accuracy", "Precision", "Recall")
resultados_ce<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados_ce)
medias_ce<-resultados_ce %>%  group_by(Covariable) %>% summarise(Accuracy = mean(Accuracy), 
                                                           Precision = mean(Precision), 
                                                           Recall = mean(Recall))
medias_ce
medias_se



###Pregunta 2: ####

## Sin Estandarizar ##

resultados_2<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    X_train=as.matrix(cv_data[[3]][[i]][c(-j,-6)])
    
    
    #Testeo
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    X_test=as.matrix(cv_data[[4]][[i]][c(-j,-6)])
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    aux<-cbind(nombres[j],i,Accuracy)
    aux_2<-rbind(aux_2,aux)
  }
  resultados_2<-rbind(resultados_2,aux_2)
}

resultados_2<-data.frame(resultados_2)
colnames(resultados_2)<-c("Covariable_fuera", "Kfold", "Accuracy")
resultados_2_se<-resultados_2 %>% mutate(Accuracy=as.numeric(Accuracy))
str(resultados_2_se)
medias_2_se<-resultados_2_se %>%  group_by(Covariable_fuera) %>% summarise(Accuracy = mean(Accuracy))


## Estandarizado ##

resultados_2<-c()
for (j in 1:5){
  nombres<-colnames(cv_data[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    X_train=cv_data[[3]][[i]][c(-j,-6)]
    
    
    #Testeo
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    X_test=cv_data[[4]][[i]][c(-j,-6)]
    
    ## Estandarizando 
    
    mux = apply(X_train,2,mean)
    sdx = apply(X_train,2,sd)
    
    xtrain_est = X_train
    xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
    xtrain_est[,2] = (X_train[,2] - mux[2])/sdx[2]
    xtrain_est[,3] = (X_train[,3] - mux[3])/sdx[3]
    xtrain_est[,4] = (X_train[,4] - mux[4])/sdx[4]
    
    xtrain_est = as.matrix(xtrain_est)
    
    xtest_est = X_test
    xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
    xtest_est[,2] = (X_test[,2] - mux[2])/sdx[2]
    xtest_est[,3] = (X_test[,3] - mux[3])/sdx[3]
    xtest_est[,4] = (X_test[,4] - mux[4])/sdx[4]
    
    xtest_est = as.matrix(xtest_est)
    X_train=xtrain_est
    X_test=xtest_est
    
    
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=1)
    
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    aux<-cbind(nombres[j],i,Accuracy)
    aux_2<-rbind(aux_2,aux)
  }
  resultados_2<-rbind(resultados_2,aux_2)
}

resultados_2<-data.frame(resultados_2)
colnames(resultados_2)<-c("Covariable_fuera", "Kfold", "Accuracy")
resultados_2_ce<-resultados_2 %>% mutate(Accuracy=as.numeric(Accuracy))
str(resultados_2_ce)
medias_2_ce<-resultados_2_ce %>%  group_by(Covariable_fuera) %>% summarise(Accuracy = mean(Accuracy))

medias_2_se
medias_2_ce

##Pregunta 3
pacman::p_load(tidyverse, tidymodels)
library("kernlab")
library(ggplot2)
library(openintro)
library(caret)
library(reshape2)
library(tidyverse)
library(dplyr)

set.seed(23)

data(starbucks)
attach(starbucks)
summary(starbucks)

data<-starbucks
data$type<- ifelse(data$type == 'bakery', 1, 0)
# Y=data$type
# data$type<-NULL
data$item<-NULL
# X=as.matrix(data)

# Prepare the data frame containing the cross validation partitions
folds <- vfold_cv(data, v = 5, strata = "type" )
folds

cv_data <- folds %>% 
  mutate(
    # Extract the train data frame for each split
    train = map(splits, ~training(.x)), 
    # Extract the validate data frame for each split
    validate = map(splits, ~testing(.x))
  )

#3 es train
cv_data[[3]][[1]][-1]
# #4 es test
# Y=as.matrix(cv_data[[3]][[1]][6])
# X=as.matrix(cv_data[[3]][[1]][1:5])


#Pregunta 3
#Vanilla svm
resultados<-c()
nombres<-colnames(cv_data[[3]][[1]])
aux_2<-c()
parametrosC <-seq(0.0000001, 1, length.out = 1100)
for (j in parametrosC){
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    X_train=cv_data[[3]][[i]][1:5]
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    
    #Testeo
    X_test=cv_data[[4]][[i]][1:5]
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    
    ## Estandarizando
    mux = apply(X_train,2,mean)
    sdx = apply(X_train,2,sd)
      
    xtrain_est = X_train
    xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
    xtrain_est = as.matrix(xtrain_est)
      
    xtest_est = X_test
    xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
    
    xtest_est = as.matrix(xtest_est)
    X_train=xtrain_est
    X_test=xtest_est
      
      
    Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
              kernel='vanilladot',scaled=FALSE,C=j)
      
    pred<-predict(Ksvm,X_test)
    Table=table(pred,Y_test)
    Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
    Precision=Table[2,2]/(Table[2,1]+Table[2,2])
    Recall=Table[2,2]/(Table[1,2]+Table[2,2])
    F1_Score=2*(Precision*Recall)/(Precision + Recall)
    aux<-cbind(j,i,Accuracy,F1_Score)
    aux_2<-rbind(aux_2,aux)
    
  }
  resultados<-rbind(resultados,aux_2)
}


#resultados de vanilla 
resultados<-data.frame(resultados)
colnames(resultados)<-c("CValue", "Kfold", "Accuracy", "F1 Score")
resultados<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  F1_Score=as.numeric(F1_Score))
str(resultados)
medias<-resultados %>%  group_by(CValue) %>% summarise(Accuracy = mean(Accuracy), 
                                                       F1_Score = mean(F1_Score))
medias

#Los valores de C que maximizar accuracy y F1 Score son:
print("Kernel Vanilla")
final1<-medias[which(medias$Accuracy==max(medias$Accuracy)&medias$F1_Score==max(medias$F1_Score)),]

plot(medias$CValue, medias$Accuracy, type='l', col=1)
lines(medias$CValue, medias$F1_Score, col=2)

#svm kernel squared exponential (Gauss)
resultados<-c()
nombres<-colnames(cv_data[[3]][[1]])
aux_2<-c()
parametrosC2 <-seq(0.0000001, 1, length.out = 110)
parametros_sigma<-seq(0.0001,5,length.out=11)

for (a in parametros_sigma){
  for (j in parametrosC2){
    for (i in 1:5){
      Kfun1=function(x,y,ls=2,sigma=a)
      {
        exp(-(1/sigma^2)*sum(abs(x-y)^2))
      }
      
      class(Kfun1)='kernel'
      
      
      aux<-c()
      #Entrenamiento
      X_train=cv_data[[3]][[i]][1:5]
      Y_train=as.matrix(cv_data[[3]][[i]][6])
      
      #Testeo
      X_test=cv_data[[4]][[i]][1:5]
      Y_test=as.matrix(cv_data[[4]][[i]][6])
      
      ## Estandarizando
      mux = apply(X_train,2,mean)
      sdx = apply(X_train,2,sd)
      
      xtrain_est = X_train
      xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
      xtrain_est = as.matrix(xtrain_est)
      
      xtest_est = X_test
      xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
      
      xtest_est = as.matrix(xtest_est)
      X_train=xtrain_est
      X_test=xtest_est
      
      
      Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
                kernel=Kfun1,scaled=FALSE,C=j)
      
      pred<-predict(Ksvm,X_test)
      Table=table(pred,Y_test)
      Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
      Precision=Table[2,2]/(Table[2,1]+Table[2,2])
      Recall=Table[2,2]/(Table[1,2]+Table[2,2])
      F1_Score=2*(Precision*Recall)/(Precision + Recall)
      aux<-cbind(a,j,i,Accuracy,F1_Score)
      aux_2<-rbind(aux_2,aux)
    }
    resultados<-rbind(resultados,aux_2)
  }
}

#resultados de Gauss 
resultados<-data.frame(resultados)
colnames(resultados)<-c("Sigma","CValue", "Kfold", "Accuracy", "F1 Score")
resultados<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  F1_Score=as.numeric(F1_Score))
str(resultados)
medias<-resultados %>%  group_by(Sigma, CValue) %>% summarise(Accuracy = mean(Accuracy), 
                                                       F1_Score = mean(F1_Score))
medias

#Los valores de C que maximizar accuracy y F1 Score son:
print("Kernel Gauss")
final2<-medias[which(medias$Accuracy==max(medias$Accuracy)&medias$F1_Score==max(medias$F1_Score)),]

#plot valores c
plot(medias$CValue, medias$Accuracy, type='l', col=1)
lines(medias$CValue, medias$F1_Score, col=2)

#plot valores sigma
plot(medias$Sigma, medias$Accuracy, type='l', col=1)
lines(medias$Sigma, medias$F1_Score, col=2)




#svm kernel locally periodic
resultados<-c()
nombres<-colnames(cv_data[[3]][[1]])
aux_2<-c()
parametrosC2 <-seq(0.0000001, 1, length.out = 6)
parametros_sigma<-seq(0.0001,5,length.out=6)
parametros_p<-seq(0.0001,5,length.out=6)
parametros_l<-seq(0.0001,5,length.out=6)


for (c in parametros_p){
  for (b in parametros_l){
    for (a in parametros_sigma){
      for (j in parametrosC2){
        for (i in 1:5){
          Kfun2=function(x,y,l=b,sigma=a,p=c)
          {
            sigma*exp(-2*sin(pi*sum(abs(x-y))/p)^2/l^2)+sigma*exp(-sum(abs(x-y)/l^2))
          }
          
          class(Kfun2)='kernel'
          
          
          aux<-c()
          #Entrenamiento
          X_train=cv_data[[3]][[i]][1:5]
          Y_train=as.matrix(cv_data[[3]][[i]][6])
          
          #Testeo
          X_test=cv_data[[4]][[i]][1:5]
          Y_test=as.matrix(cv_data[[4]][[i]][6])
          
          ## Estandarizando
          mux = apply(X_train,2,mean)
          sdx = apply(X_train,2,sd)
          
          xtrain_est = X_train
          xtrain_est[,1] = (X_train[,1] - mux[1])/sdx[1]
          xtrain_est = as.matrix(xtrain_est)
          
          xtest_est = X_test
          xtest_est[,1] = (X_test[,1] - mux[1])/sdx[1]
          
          xtest_est = as.matrix(xtest_est)
          X_train=xtrain_est
          X_test=xtest_est
          
          
          Ksvm=ksvm(y=as.factor(Y_train),x=X_train,
                    kernel=Kfun2,scaled=FALSE,C=j)
          
          pred<-predict(Ksvm,X_test)
          Table=table(pred,Y_test)
          Accuracy=(Table[1,1]+Table[2,2])/sum(Table)
          Precision=Table[2,2]/(Table[2,1]+Table[2,2])
          Recall=Table[2,2]/(Table[1,2]+Table[2,2])
          F1_Score=2*(Precision*Recall)/(Precision + Recall)
          aux<-cbind(c,b,a,j,i,Accuracy,F1_Score)
          aux_2<-rbind(aux_2,aux)
          print(a)
        }
        resultados<-rbind(resultados,aux_2)
      }
    }
  }
}
#resultados de locally periodic
resultados<-data.frame(resultados)
colnames(resultados)<-c("PValue","LValue","Sigma","CValue", "Kfold", "Accuracy", "F1 Score")
resultados<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  F1_Score=as.numeric(F1_Score))
str(resultados)
medias<-resultados %>%  group_by(PValue, LValue, Sigma, CValue) %>% summarise(Accuracy = mean(Accuracy), 
                                                      F1_Score = mean(F1_Score))
medias

#Los valores de C que maximizar accuracy y F1 Score son:
print("Kernel locally periodic")
final3<-medias[which(medias$Accuracy==max(medias$Accuracy)&medias$F1_Score==max(medias$F1_Score)),]


#plot valores c
plot(medias$CValue, medias$Accuracy, type='l', col=1)
lines(medias$CValue, medias$F1_Score, col=2)

#plot valores sigma
plot(medias$Sigma, medias$Accuracy, type='l', col=1)
lines(medias$Sigma, medias$F1_Score, col=2)

#plot valores l
plot(medias$LValue, medias$Accuracy, type='l', col=1)
lines(medias$LValue, medias$F1_Score, col=2)

#plot valores p
plot(medias$PValue, medias$Accuracy, type='l', col=1)
lines(medias$PValue, medias$F1_Score, col=2)

print(final1)
print(final2)
print(final3)
