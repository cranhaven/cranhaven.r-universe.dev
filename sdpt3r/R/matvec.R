matvec <- function(A,x){
  m <- max(dim(A$mat11))
  m2 <- length(x)-m
  if(m2 > 0){
    x1 <- x[1:m]
  }else{
    x1 <- x
  }
  Ax <- mexMatvec(A$mat11,x1)
  if(m2 > 0){
    x2 <- x[m+c(1:m2)]
    Ax <- Ax + mexMatvec(A$mat12,x2)
    Ax2 <- mexMatvec(A$mat12,x1,1) + mexMatvec(A$mat22,x2)
    Ax <- rbind(as.matrix(Ax),as.matrix(Ax2))
  }
  return(Ax)
}