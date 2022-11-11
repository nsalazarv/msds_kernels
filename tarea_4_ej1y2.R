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
cv_data[[3]][[1]][-1]
# #4 es test
# 
# Y=as.matrix(cv_data[[3]][[1]][6])
# X=as.matrix(cv_data[[3]][[1]][1:5])


#Pregunta 1.

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
resultados<-resultados %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados)
medias<-resultados %>%  group_by(Covariable) %>% summarise(Accuracy = mean(Accuracy), 
                                                   Precision = mean(Precision), 
                                                   Recall = mean(Recall))
medias
#accuracy: proteina y carb; precision carb y recall protein

#Pregunta 2:
nombres<-colnames(cv_data[[3]][[1]])
nombres

cv_data_2<-cv_data
for (z in 1:5){
  cv_data_2[[3]][[z]][6]<-NULL
  cv_data_2[[4]][[z]][6]<-NULL
  
}
resultados_2<-c()
for (j in 1:5){
  nombres<-colnames(cv_data_2[[3]][[1]])
  aux_2<-c()
  for (i in 1:5){
    aux<-c()
    #Entrenamiento
    Y_train=as.matrix(cv_data[[3]][[i]][6])
    cv_data_2[[3]][[i]][6]<-NULL
    X_train=as.matrix(cv_data_2[[3]][[i]][-j])  
    
    
    #Testeo
    Y_test=as.matrix(cv_data[[4]][[i]][6])
    cv_data_2[[4]][[i]][6]<-NULL
    X_test=as.matrix(cv_data_2[[4]][[i]][-j])
    
    
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
  resultados_2<-rbind(resultados_2,aux_2)
}

resultados_2<-data.frame(resultados_2)
colnames(resultados_2)<-c("Covariable_fuera", "Kfold", "Accuracy", "Precision", "Recall")
resultados_2<-resultados_2 %>% mutate(Accuracy=as.numeric(Accuracy), 
                                  Precision=as.numeric(Precision),
                                  Recall=as.numeric(Recall))
str(resultados_2)
medias_2<-resultados_2 %>%  group_by(Covariable_fuera) %>% summarise(Accuracy = mean(Accuracy), 
                                                           Precision = mean(Precision), 
                                                           Recall = mean(Recall))
medias_2

