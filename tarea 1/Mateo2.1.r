funcion=function(A,B)
{
  n1=nrow(A)
  n2=ncol(A)
  n3=nrow(B)
  n4=ncol(B)
  
  if(n1==n2 && n1==n3 && n4==50){
    C=matrix(rep(0, nrow(a)*ncol(b)), nrow = nrow(a))
    for(i in 1:nrow(a)){
      for(j in 1:ncol(b)){
        for(k in 1:nrow(b)){
          c[i,j] <- c[i,j] + a[i,k]*b[k, j]
        }
      }
    }
    return(C)
  }
  else{
    return("No se cumplen las condiciones")
  }
  
} 
