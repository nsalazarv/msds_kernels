### Script pregunta 3

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
y=train[["protein"]]
w_manual=solve(t(X)%*%X)%*%t(X)%*%y
w_manual

#Revisamos si w y w_manual tienen los mismos valores
w
w_manual


# Estimando los coeficientes de regresión con la función lm()

modelo2 = lm(protein~calories,data=train)
summary(modelo2)

w2 = modelo2[["coefficients"]]
w2

# Estandarizando la columna calories

processTrain = train
processTest = test

pre_proc_val <- preProcess(processTrain[,'calories'], method = c("center", "scale"))

processTrain[,'calories'] = predict(pre_proc_val, processTrain[,'calories'])
processTest[,'calories'] = predict(pre_proc_val, processTest[,'calories'])

# Repitiendo OLS con la columna calories estandarizada


