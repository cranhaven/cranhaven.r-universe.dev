qprod <- function(blk,p,A,x){
  
  if(is(x, "sparseMatrix")){
    x <- matrix(x)
  }
  
  n <- max(dim(as.matrix(x)))
  ii <- c(1:n)
  jj <- mexexpand(blk[[p,2]], matrix(1:max(dim(as.matrix(blk[[p,2]])))))
  X <- matrix(0, n, max(jj))
  for(i in 1:n){
    X[ii[i],jj[i]] <- x[i]
  }
  Ax <- A %*% X
  return(Ax)
}