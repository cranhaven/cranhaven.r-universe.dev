checkdense <- function(A){
  m <- nrow(A)
  n <- ncol(A)
  idxden <- c()
  nzratio <- 1
  
  if(m > 1000){
    nzratio <- .2
  }
  if(m > 2000){
    nzratio <- .1
  }
  if(m > 5000){
    nzratio <- 0.05
  }
  if(nzratio < 1){
    ind <- which(A != 0)
    Aprime <- matrix(0,nrow=m,ncol=n)
    Aprime[ind] <- 1
    nzcolA <- colSums(Aprime)
    idxden <- which(nzcolA > nzratio*m)
    if(length(idxden) > max(200,0.1*n)){
      idxden <- c()
    }
  }
  return(idxden)
}