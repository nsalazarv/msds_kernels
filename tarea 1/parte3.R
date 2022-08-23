### Script pregunta 3

rm(list=ls())
pacman::p_load(tidyverse, openintro, ggplot2, caret)
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
summary(modelo2)

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
ecmp





