findcoeffsub <- function(blk, At, idxB, idxN){
  AB <- c()
  AN <- c()
  for(p in 1:nrow(blk)){
    AB <- rbind(AB, At[[p,1]][,idxB])
    AN <- rbind(AB, At[[p,1]][,idxN])
  }
  m <- nrow(AB)
  n <- ncol(AB)
  
  out <- Matrix::lu(Matrix(AB), sparse=TRUE)
  L <- out@L
  U <- out@U
  p <- out@p
  q <- out@q
  
  #Find W s.t AN = AB*W
  rhs <- AN[p+1,]
  Lhat <- L[1:n,]
  W <- (solve(U, (solve(Lhat, rhs[1:n,]))))[,q]
  resnorm <- norm(AN - AB %*% W, type="F")/max(1, norm(AN,type="F"))
  
  return(list(W=W, resnorm=resnorm))
}
