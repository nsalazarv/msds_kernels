### Script P2 Tarea 2 Kernels

rm(list = ls())
set.seed(23)

path = '~/OneDrive - Universidad Adolfo Ibanez/Codes/Universidad/Kernels/Repo/Tarea 2/sim.txt'
data = read.delim(path, header = TRUE, sep = ",", dec = ".")

data = data[sample(nrow(data)),]

### Pregunta 1

# Estableciendo 10-fold CV

folds <- cut(seq(1, nrow(data)), breaks = 10, labels=FALSE)

ecm_vec = c()
rsq_vec = c()

for(i in 1:10){
  
  print(paste("Fold: ",i))
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  
  x_train = train[1:2]
  y_train = unlist(train[3])
  
  x_test = test[1:2]
  y_test = unlist(test[3])
  
  # Estandarizando 
  
  mux = apply(x_train,2,mean)
  sdx = apply(x_train,2,sd)
  
  xtrain_est = x_train
  xtrain_est[,1] = (x_train[,1] - mux[1])/sdx[1]
  xtrain_est[,2] = (x_train[,2] - mux[2])/sdx[2]
  
  xtest_est = x_test
  xtest_est[,1] = (x_test[,1] - mux[1])/sdx[1]
  xtest_est[,2] = (x_test[,2] - mux[2])/sdx[2]
  
  # Función de Kernel
  
  Kfun = function(xi,xj,l2=0.25)
  {
    exp(-sum((xi-xj)^2)/l2)
  }
  
  # Matriz de Gram
  
  ntrain = dim(xtrain_est)[1]
  K = matrix(0, nrow = ntrain, ncol = ntrain)
  
  for(i in 1:ntrain)
  {
    for(j in 1:ntrain)
    {
      K[i,j] = Kfun(xtrain_est[i,], xtrain_est[j,])
    }
  }
  
  # Calculando W hat e Y hat
  
  lambda = 1
  w_hat = solve(t(K)%*%K+lambda*diag(ntrain))%*%t(K)%*%y_train
  
  y_hat = K%*%w_hat
  
  # Revisando valores mínimos y máximos para armar parrilla
  
  min(data[1]) # 0.06019702
  max(data[1]) # 4.976222
  
  min(data[2]) # 0.01729765
  max(data[2]) # 4.985938
  
  s = seq(0, 5, 0.05)
  sgrid = as.matrix(expand.grid(s,s))
  
  sgrid_est = sgrid
  sgrid_est[,1]=(sgrid_est[,1] - mux[1])/sdx[1]
  sgrid_est[,2]=(sgrid_est[,2] - mux[2])/sdx[2]
  
  ## Armando predicciones con el dataset de entrenamiento
  
  #xtrain_est = unlist(xtrain_est)
  npred = dim(sgrid_est)[1]
  K_pred_train = matrix(0, nrow = npred, ncol = ntrain)
  
  
  for(i in 1:ntrain)
  {
    K_pred_train[,i]=exp(-rowSums((sgrid_est - matrix(unlist(xtrain_est[i,]), nrow = npred, ncol = 2, byrow = TRUE))^2)/0.25)
  }
  
  y_pred_train = K_pred_train%*%w_hat
  
  ## Armando predicciones con el dataset de testeo
  
  ntest = dim(xtest_est)[1]
  K_pred_test=matrix(0, nrow = ntest, ncol = ntrain)
  for(i in 1:ntrain)
  {
    K_pred_test[,i]=exp(-rowSums((xtest_est - matrix(unlist(xtrain_est[i,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/0.25)
  }
  
  y_pred_test = K_pred_test%*%w_hat
  
  ## Error cuadrático medio y R cuadrado
  
  ecm = 1/ntest*sum((y_test - y_pred_test)^2)
  ecm_vec = append(ecm_vec,ecm)
  
  rsq = cor(y_test, y_pred_test)^2
  rsq_vec = append(rsq_vec, rsq)
}

ecm_vec
rsq_vec

#################################  #################################  ################################# 
################################# Pregunta 2 #################################

l_vec = c()
lam_vec = c()
ecm_vec2 = c()
rsq_vec2 = c()

l_grid = seq(0.1, 1, 0.1)
lam_grid = seq(0.1, 1, 0.1)

for(i in 1:10){
  
  print(paste("Fold: ", i))
  
  llgrid = expand.grid(l_grid, lam_grid)
  dim_grid = dim(llgrid)[1]
  
  ecm_save = rep(0, dim_grid)
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  
  x_train = train[1:2]
  y_train = unlist(train[3])
  
  x_test = test[1:2]
  y_test = unlist(test[3])
  
  # Estandarizando 
  
  mux = apply(x_train,2,mean)
  sdx = apply(x_train,2,sd)
  
  xtrain_est = x_train
  xtrain_est[,1] = (x_train[,1] - mux[1])/sdx[1]
  xtrain_est[,2] = (x_train[,2] - mux[2])/sdx[2]
  
  xtest_est = x_test
  xtest_est[,1] = (x_test[,1] - mux[1])/sdx[1]
  xtest_est[,2] = (x_test[,2] - mux[2])/sdx[2]
  
  # Función de Kernel
  
  Kfun = function(xi,xj,l)
  {
    exp(-sum((xi-xj)^2)/(l^2))
  }
  
  # Obteniendo l y lambda
  
  for(z in 1:dim_grid){
    
    print(paste("Iter: ", z))
    
    ntrain = dim(xtrain_est)[1]
    K = matrix(0, nrow = ntrain, ncol = ntrain)
    
    for(j in 1:ntrain){
      for(k in 1:ntrain){
        K[j,k] = Kfun(xtrain_est[j,], xtrain_est[k,], llgrid[z,1])
      }
    }
    
    w_hat = solve(t(K)%*%K+llgrid[z,2]*diag(ntrain))%*%t(K)%*%y_train
    
    ntest = dim(xtest_est)[1]
    K_pred_test=matrix(0, nrow = ntest, ncol = ntrain)
    
    for(j in 1:ntrain){
      K_pred_test[,j]=exp(-rowSums((xtest_est - matrix(unlist(xtrain_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(llgrid[z,1]^2))
    }
    
    y_pred_test = K_pred_test%*%w_hat
    
    ecm_save[z] = 1/ntest*sum((y_test - y_pred_test)^2)
  }
  
  #Encontramos los parámetros óptimos:
  
  index = which(ecm_save == min(ecm_save))
  llgrid[index,]
  
  l_vec = append(l_vec, llgrid[index,][1])
  lam_vec = append(lam_vec, llgrid[index,][2])
  
  ## Calculando en base a los valores obtenidos
  
  print(l_vec[i])
  print(lam_vec[i])
  
  ntrain = dim(xtrain_est)[1]
  K = matrix(0, nrow = ntrain, ncol = ntrain)
  
  for(j in 1:ntrain){
    for(k in 1:ntrain){
      K[j,k] = Kfun(xtrain_est[j,], xtrain_est[k,], unlist(l_vec[i]))
    }
  }
  
  w_hat = solve(t(K)%*%K+unlist(lam_vec[i])*diag(ntrain))%*%t(K)%*%y_train
  
  ntest = dim(xtest_est)[1]
  K_pred_test=matrix(0, nrow = ntest, ncol = ntrain)
  
  for(j in 1:ntrain){
    K_pred_test[,j]=exp(-rowSums((xtest_est - matrix(unlist(xtrain_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(unlist(l_vec[i])^2))
  }
  
  y_pred_test = K_pred_test%*%w_hat
  
  ecm = 1/ntest*sum((y_test - y_pred_test)^2)
  ecm_vec2 = append(ecm_vec2,ecm)
  
  rsq = cor(y_test, y_pred_test)^2
  rsq_vec2 = append(rsq_vec2, rsq)
  print(rsq_vec2)
  print(ecm_vec2)
}  

l_vec
lam_vec
ecm_vec2
rsq_vec2

#2.3
boxplot(unlist(l_vec), main='L')
boxplot(unlist(lam_vec), main='Lambda')
#Tenemos que los valores de LS tienen mayor varianza en sus distribucion en contraste con los valores lambda que se mantienen en un rango mucho menor, teniendo diferencias considerables al caso previo

#2.4
l1_vec = c()
l2_vec = c()
lam_vec = c()
ecm_vec4 = c()
rsq_vec4 = c()

l1_grid = seq(0.1, 1, 0.1)
l2_grid = seq(0.1, 1, 0.1)
lam_grid = seq(0.1, 1, 0.1)

for(i in 1:10){
  
  print(paste("Fold: ", i))
  
  llgrid = expand.grid(l1_grid,l2_grid, lam_grid)
  dim_grid = dim(llgrid)[1]
  
  ecm_save = rep(0, dim_grid)
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  
  x_train = train[1:2]
  y_train = unlist(train[3])
  
  x_test = test[1:2]
  y_test = unlist(test[3])
  
  # Estandarizando 
  
  mux = apply(x_train,2,mean)
  sdx = apply(x_train,2,sd)
  
  xtrain_est = x_train
  xtrain_est[,1] = (x_train[,1] - mux[1])/sdx[1]
  xtrain_est[,2] = (x_train[,2] - mux[2])/sdx[2]
  
  xtest_est = x_test
  xtest_est[,1] = (x_test[,1] - mux[1])/sdx[1]
  xtest_est[,2] = (x_test[,2] - mux[2])/sdx[2]
  
  
  #############################PRobado hasta aka
  # Función de Kernel
  
  Kfun = function(xi,xj,l1,l2)
  {
    exp(((-((xi-xj)/l1)^2)-((xi-xj)/l2)^2))
  }
  
  # Obteniendo l y lambda
  
  for(z in 1:dim_grid){
    for(y in 1:dim_grid){
    
    print(paste("Iter: ", z))
    
    ntrain = dim(xtrain_est)[1]
    K = array(0, c(ntrain, ntrain,ntrain))
    
    for(j in 1:ntrain){
      for(k in 1:ntrain){
        K[j,k,z] = Kfun(xtrain_est[j,], xtrain_est[k,], llgrid[z,1],llgrid[z,2])
        }
      }
    }
    
    w_hat = solve(t(K)%*%K+llgrid[z,3]*diag(ntrain))%*%t(K)%*%y_train
    
    ntest = dim(xtest_est)[1]
    K_pred_test=matrix(0, nrow = ntest, ncol = ntrain)
    
    for(j in 1:ntrain){
      K_pred_test[,j]=exp(-rowSums((xtest_est - matrix(unlist(xtrain_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(llgrid[z,1]^2))
    }
    
    y_pred_test = K_pred_test%*%w_hat
    
    ecm_save[z] = 1/ntest*sum((y_test - y_pred_test)^2)
  }
  
  #Encontramos los parámetros óptimos:
  
  index = which(ecm_save == min(ecm_save))
  llgrid[index,]
  
  l1_vec = append(l1_vec, llgrid[index,][1])
  l2_vec = append(l2_vec, llgrid[index,][2])
  lam_vec = append(lam_vec, llgrid[index,][3])
  
  ## Calculando en base a los valores obtenidos
  
  print(l1_vec[i])
  print(l2_vec[i])
  print(lam_vec[i])
  
  ntrain = dim(xtrain_est)[1]
  K = matrix(0, nrow = ntrain, ncol = ntrain)
  
  for(j in 1:ntrain){
    for(k in 1:ntrain){
      K[j,k] = Kfun(xtrain_est[j,], xtrain_est[k,], unlist(l1_vec[i]), unlist(l2_vec[i]))
    }
  }
  
  w_hat = solve(t(K)%*%K+unlist(lam_vec[i])*diag(ntrain))%*%t(K)%*%y_train
  
  ntest = dim(xtest_est)[1]
  K_pred_test=matrix(0, nrow = ntest, ncol = ntrain)
  
  for(j in 1:ntrain){
    K_pred_test[,j]=exp(-rowSums((xtest_est - matrix(unlist(xtrain_est[j,]), nrow = ntest, ncol = 2, byrow = TRUE))^2)/(unlist(l_vec[i])^2))
  }
  
  y_pred_test = K_pred_test%*%w_hat
  
  ecm = 1/ntest*sum((y_test - y_pred_test)^2)
  ecm_vec4 = append(ecm_vec4,ecm)
  
  rsq = cor(y_test, y_pred_test)^2
  rsq_vec4 = append(rsq_vec4, rsq)
  print(rsq_vec4)
  print(ecm_vec4)
}  

l1_vec
l2_vec
lam_vec
ecm_vec4
rsq_vec4
