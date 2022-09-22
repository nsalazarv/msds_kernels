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

x1_train = unlist(train[1])
x2_train = unlist(train[2])
y_train = unlist(train[3])

x1_test = unlist(test[1])
x2_test = unlist(test[2])
y_test = unlist(test[3])

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

# Matriz de Gram

ntrain = 151
K = matrix(0,nrow=ntrain,ncol=ntrain)

for(i in 1:ntrain)
{
  for(j in 1:ntrain)
  {
    K[i,j] = Kfun(x1train_est[i], x2train_est[j])
  }
}

# Calculando W hat e Y hat

lambda = 1
In=diag(ntrain)
w_hat = solve(K + lambda*In)%*%y_train

y_hat=K%*%w_hat

