precond <- function(A,L,x){
  
  m <- L$matdim
  m2 <- length(x) - m
  Mx <- matrix(0,length(x),1)
  
  for(iter in 1:1){
    r <- x
    r1 <- as.matrix(r[1:m])
    if(m2 > 0){
      r2 <- r[m+c(1:m2)]
      w <- linsysolvefun(L,r1)
      z <- mexMatvec(A$mat12,w,1) - r2
      z <- solve(L$Mu,solve(L$Ml, L$Mp %*% z))
      r1 <- r1 - mexMatvec(A$mat12,z)
    }
    d <- linsysolvefun(L,r1)
    if(m2 > 0){
      d <- rbind(d,z)
    }
    Mx <- Mx + d
  }
  return(Mx)
}