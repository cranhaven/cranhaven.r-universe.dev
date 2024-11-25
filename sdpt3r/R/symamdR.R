symamdR <- function(M){
  
  if(nrow(M) != ncol(M)){
    stop("M must be a symmerix matrix")
  }
  
  M.lowertri <- M[lower.tri(M, diag=FALSE)]
  M.temp <- M
  M.temp[upper.tri(M, diag=TRUE)] <- 0
  
  B_N <- nrow(M)
  B_NNZ <- length(which(M.lowertri != 0))
  q <- rep(0, B_N+1)
  B <- c()
  
  for(i in 1:B_N){
    tmp <- which(M.temp[,i] != 0)
    B <- c(B, tmp)
    q[i+1] <- q[i] + length(tmp) 
  }
  B <- B - 1 #To make indexing 0 based
  
  perm <- numeric(B_N+1)
  
  B_N <- as.integer(B_N)
  B <- as.integer(B)
  q <- as.integer(q)
  perm <- as.integer(perm)
  
  Out <- .C("symamdWrapper",B_N, B, q, perm, numeric(1))
  Out <- Out[[4]] + 1
  Out <- Out[-length(Out)]
  
  return(Out)

}