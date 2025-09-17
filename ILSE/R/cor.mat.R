cor.mat <- function(p, rho, type='toeplitz'){

  mat <- diag(p)
  if(type=='toeplitz'){
    for(i in 2:p){
      for(j in 1:i){
        mat[i,j] <- mat[j,i] <- rho^(abs(i-j))
      }
    }
  }
  if(type=='identity'){
    mat[mat==0] <- rho
  }
  return(mat)
}
