cov.mat <- function(sdvec, rho, type='toeplitz'){
  p <- length(sdvec)
  cormat <- cor.mat(p, rho, type)
  covmat <- matrix(0, p, p)
  for(i in 1:p){
    for(j in 1:p){
      covmat[i,j] <- sdvec[i]*sdvec[j]*cormat[i,j]
    }
  }
  return(covmat)
}
