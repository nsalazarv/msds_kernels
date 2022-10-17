### Script P3 Tarea 2 Kernels
rm(list = ls())
set.seed(23)
library("rgl")

path = "C:/Users/Dell/Desktop/datos.txt"
data = read.delim(path, header = TRUE, sep = ",", dec = ".")

data = data[sample(nrow(data)),]

##Ejercicio 2

#Pregunta A

lambda=0.01

#Definimos el kernel Squared - exponential
Kfun1 = function(xi,xj,l)
{
  exp(-sum((xi-xj)^2)/(l^2))
}

#Definimos el kernel browniano
Kfun2 = function(xi,xj,o)
{
  o^2*min(xi,xj)
}


l_vec = c()
o_vec = c()
ecm_vec1 = c()
rsq_vec1 = c()
ecm_vec2 = c()
rsq_vec2 = c()


l_grid = seq(0.1, 1, 0.1)
o_grid = seq(0.1, 1, 0.1)

llgrid = expand.grid(l_grid, o_grid)
dim_grid = dim(llgrid)[1]

ecm_save = rep(0, dim_grid)


x = data[1]
y = unlist(data[2])

x_test = data[1]
y_test = unlist(data[2])

# Estandarizando 

mux = apply(x,2,mean)
sdx = apply(x,2,sd)


x_est = x
x_est[,1] = (x_train[,1] - mux[1])/sdx[1]

# Obteniendo l y o

for(z in 1:dim_grid){
  
  print(paste("Iter: ", z))
  
  ntrain = dim(x_est)[1]
  K1 = matrix(0, nrow = ntrain, ncol = ntrain)
  K2 = matrix(0, nrow = ntrain, ncol = ntrain)
  
  for(j in 1:ntrain){
    for(k in 1:ntrain){
      K1[j,k] = Kfun1(x_est[j,], x_est[k,], llgrid[z,1])
      K2[j,k] = Kfun2(x_est[j,], x_est[k,], llgrid[z,2])
    }
  }
  
  w_hat1 = solve(t(K1)%*%K1+lambda*diag(ntrain))%*%t(K1)%*%y
  w_hat2 = solve(t(K2)%*%K2+lambda*diag(ntrain))%*%t(K2)%*%y
  
  ntest = dim(x_est)[1]
  K_pred_test1=matrix(0, nrow = ntest, ncol = ntrain)
  K_pred_test2=matrix(0, nrow = ntest, ncol = ntrain)
  
  for(j in 1:ntrain){
    K_pred_test1[,j]=exp(-rowSums((x_est - matrix(unlist(x_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(llgrid[z,1]^2))
    K_pred_test2[,j]=exp(-rowSums((x_est - matrix(unlist(x_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(llgrid[z,2]^2))
    
    }
  
  y_pred_test1 = K_pred_test1%*%w_hat1
  y_pred_test2 = K_pred_test2%*%w_hat2
  
  ecm_save[z] = 1/ntest*sum((y_test - y_pred_test1)^2)
}
