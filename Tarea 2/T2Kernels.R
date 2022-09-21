### Script P2 Tarea 2 Kernels

rm(list = ls())
set.seed(23)

path = '~/OneDrive - Universidad Adolfo Ibanez/Codes/Universidad/Kernels/Repo/Tarea 2/sim.txt'
data = read.delim(path, header = TRUE, sep = ",", dec = ".")

# Creando datasets de prueba y entrenamiento

index <- sample(1:4, 200, replace=TRUE) 
data['grupo'] <- index

train <- data[which(data$grupo == 1 | data$grupo == 2 | data$grupo == 3),]
test <- data[which(data$grupo == 4),]

x1_train = train[1]
x2_train = train[2]
y_train = train[3]

x1_test = test[1]
x2_test = test[2]
y_test = test[3]

# Estandarizando 

mux1 = mean(x1_train)
sdx1 = sd(x1_train)
mux2 = mean(x2_train)
sdx2 = sd(x2_train)

x1train_est = (x1_train - mux1)/sdx1
x1test_est = (x1_test - mux1)/sdx1
x2train_est = (x2_train - mux2)/sdx2
x2test_est = (x2_test - mux2)/sdx2

# Función de Kernel

Kfun = function(xi,xj,l2=0.5)
{
  exp(-sum((xi-xj)^2)/l2)
}

