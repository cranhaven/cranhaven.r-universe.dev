ops <- function(X, option, Y, alpha=1){
  
  if(option == "norm"){
    if(!is.list(X)){
      Z <- sqrt(sum(X*X))
    }else{
      Z <- 0
      for(p in 1:max(dim(X))){
        Z <- Z + sum(as.numeric(X[[p]]*X[[p]]))
      }
      Z <- sqrt(Z)
    }
  }
  
  if(option == "identity"){
    blk <- X
    Z <- matrix(list(), nrow(blk),1)
    for(p in 1:nrow(blk)){
      n <- sum(blk[[p,2]])
      if(blk[[p,1]] == "s"){
        Z[[p,1]] <- diag(1,n,n)
      }else if(blk[[p,1]] == "q"){
        s <- 1 + c(0,cumsum(blk[[p,2]]))
        len <- length(blk[[p,2]])
        Z[[p,1]] <- rep(0,n)
        Z[[p,1]][s[1:len]] <- rep(1,len)
      }else if(blk[[p,1]] == "l"){
        Z[[p,1]] <- rep(1,n)
      }else if(blk[[p,1]] == "u"){
        Z[[p,1]] <- rep(0,n)
      }
    }
  }
  
  if(option == "+"){
    if(!is.list(X) & !is.list(Y)){
      Z <- X + alpha*Y
    }else if(is.list(X) & is.list(Y)){
      if(length(X) == 1){
        Z <- matrix(list(),1,1)
        nRowX <- 1
      }else{
        Z <- matrix(list(),nrow(X),ncol(X))
        nRowX <- nrow(X)
      }
      if(length(alpha) == 1){
        alpha <- alpha*rep(1,nRowX)
      }
      for(p in 1:nRowX){
        Z[[p,1]] <- X[[p,1]] + alpha[p]*Y[[p,1]]
      }
    }else if(is.list(X) & !is.list(Y)){
      if(length(Y) == 1){
        Y <- Y*matrix(1,nrow(X),1)
      }
      if(length(X) == 1){
        Z <- matrix(list(),1,1)
        nRowX <- 1
      }else{
        Z <- matrix(list(),nrow(X),ncol(X))
        nRowX <- nrow(X)
      }
      for(p in 1:nRowX){
        Z[[p,1]] <- X[[p,1]] + Y[p,1]
      }
    }else if(!is.list(X) & is.list(Y)){
      if(length(X) == 1){
        X <- X*matrix(1,nrow(Y),1)
      }
      Z <- matrix(list(),nrow(Y),ncol(Y))
      for(p in 1:nrow(Y)){
        Z[[p,1]] <- Y[[p,1]] + X[p,1]
      }
    }
  }
  
  if(option == "-"){
    if(!is.list(X) & !is.list(Y)){
      Z <- X - alpha*Y
    }else if(is.list(X) & is.list(Y)){
      if(length(X) == 1){
        Z <- matrix(list(),1,1)
        nRowX <- 1
      }else{
        Z <- matrix(list(),nrow(X),ncol(X))
        nRowX <- nrow(X)
      }
      if(length(alpha) == 1){
        alpha <- alpha*rep(1,nRowX)
      }
      for(p in 1:nRowX){
        Z[[p,1]] <- X[[p,1]] - alpha[p]*Y[[p,1]]
      }
    }else if(is.list(X) & !is.list(Y)){
      if(length(Y) == 1){
        Y <- Y*matrix(1,nrow(X),1)
      }
      if(length(X) == 1){
        Z <- matrix(list(),1,1)
        nRowX <- 1
      }else{
        Z <- matrix(list(),nrow(X),ncol(X))
        nRowX <- nrow(X)
      }
      for(p in 1:nRowX){
        Z[[p,1]] <- X[[p,1]] - Y[p,1]
      }
    }else if(!is.list(X) & is.list(Y)){
      if(length(X) == 1){
        X <- X*matrix(1,nrow(Y),1)
      }
      Z <- matrix(list(),nrow(Y),ncol(Y))
      for(p in 1:nrow(Y)){
        Z[[p,1]] <- Y[[p,1]] - X[p,1]
      }
    }
  }
  
  if(option == "*"){
    if(!is.list(X) & !is.list(Y)){
      Z <- X * alpha*Y
    }else if(is.list(X) & is.list(Y)){
      Z <- matrix(list(),nrow(X),ncol(X))
      if(length(alpha) == 1){
        alpha <- alpha*rep(1,nrow(X))
      }
      for(p in 1:nrow(X)){
        Z[[p,1]] <- X[[p,1]] * alpha[p]*Y[[p,1]]
      }
    }else if(is.list(X) & !is.list(Y)){
      if(length(Y) == 1){
        Y <- Y*matrix(1,nrow(X),1)
      }
      Z <- matrix(list(),nrow(X),ncol(X))
      for(p in 1:nrow(X)){
        Z[[p,1]] <- X[[p,1]] * Y[p,1]
      }
    }else if(!is.list(X) & is.list(Y)){
      if(length(X) == 1){
        X <- X*matrix(1,nrow(Y),1)
      }
      Z <- matrix(list(),nrow(Y),ncol(Y))
      for(p in 1:nrow(Y)){
        Z[[p,1]] <- Y[[p,1]] * X[p,1]
      }
    }
  }
  
  if(option == "/"){
    if(!is.list(X) & !is.list(Y)){
      Z <- X / alpha*Y
    }else if(is.list(X) & is.list(Y)){
      Z <- matrix(list(),nrow(X),ncol(X))
      if(length(alpha) == 1){
        alpha <- alpha*rep(1,nrow(X))
      }
      for(p in 1:nrow(X)){
        Z[[p,1]] <- X[[p,1]] / alpha[p]*Y[[p,1]]
      }
    }else if(is.list(X) & !is.list(Y)){
      if(length(Y) == 1){
        Y <- Y*matrix(1,nrow(X),1)
      }
      Z <- matrix(list(),nrow(X),ncol(X))
      for(p in 1:nrow(X)){
        Z[[p,1]] <- X[[p,1]] / Y[p,1]
      }
    }else if(!is.list(X) & is.list(Y)){
      if(length(X) == 1){
        X <- X*matrix(1,nrow(Y),1)
      }
      Z <- matrix(list(),nrow(Y),ncol(Y))
      for(p in 1:nrow(Y)){
        Z[[p,1]] <- Y[[p,1]] / X[p,1]
      }
    }
  }
  
  if(option == "getM"){
    if(!is.list(X)){
      Z <- nrow(X)
    }else{
      Z <- c()
      for(p in 1:max(dim(X))){
        Z <- c(Z,nrow(X[[p,1]]))
      }
      Z <- sum(Z)
    }
  }
  
  if(option == "inv"){
    if(!is.list(X)){
      m <- nrow(X)
      n <- ncol(X)
      n2 <- n^2
      if(m == n){
        Z <- solve(X)
      }else if(m > 1 & n == 1){
        Z <- 1/X
      }
    }else{
      Z <- matrix(list(),nrow(X),ncol(X))
      for(p in 1:length(X)){
        m <- nrow(X[[p]])
        n <- ncol(X[[p]])
        n2 <- n
        if(m == n){
          Z[[p,1]] <- solve(X[[p,1]])
        }else if(m > 1 & n == 1){
          Z[[p,1]] <- 1/X[[p,1]]
        }
      }
    }
  }
  
  if(option == "abs"){
    if(!is.list(X)){
      Z <- abs(X)
    }else{
      Z <- matrix(list(),nrow(X),ncol(X))
      for(p in 1:max(dim(X))){
        Z[[p]] <- abs(X[[p]])
      }
    }
  }
  
  if(option == "max"){
    if(!is.list(X)){
      Z <- max(X)
    }else{
      Z <- c()
      for(p in 1:max(dim(X))){
        Z<- c(Z,max(X[[p]]))
      }
      Z <- max(Z)
    }
  }
  
  return(Z)
}