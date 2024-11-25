snlKdag <- function(D){
  
  nn <- max(nrow(D),ncol(D))
  
  D <- .5*D
  vn <- colSums(D)/nn
  evnn <- sum(vn)/nn
  
  B <- matrix(rep(0,nn^2),nrow=nn)
  
  for(ii in 1:nn){
    for(jj in 1:nn){
      B[ii,jj] <- vn[ii] + vn[jj]
    }
  }
  
  B <- B - D
  B <- B - evnn
  
  return(B)
}