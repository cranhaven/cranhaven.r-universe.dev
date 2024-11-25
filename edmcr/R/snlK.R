snlK <- function(B){
  
  D <- matrix(rep(0,nrow(B)*ncol(B)), nrow=nrow(B))
  
  for(ii in 1:max(nrow(B),ncol(B))){
    for(jj in 1:max(nrow(B),ncol(B))){
      D[ii,jj] <- B[ii,ii] + B[jj,jj] - 2*B[ii,jj]
    }
  }
  
  return(D)
}