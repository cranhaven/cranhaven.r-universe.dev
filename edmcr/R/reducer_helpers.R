pcarank <- function(X){
  sv <- pcab(X)$sv
  out <- length(sv)
  return(out)
}

pcab <- function(X){
  
  dim <- nrow(X)
  num <- ncol(X)
  
  mX <- rowMeans(X)
  cX <- X - mX
  
  if(dim < num){
    C <- cov.wt(t(cX))$cov*((nrow(t(cX))-1)/nrow(t(cX)))
    out <- eigb((C+t(C))/2)
    V <- out$V
    S <- out$D
    
    sv <- S
    nz <- (sv/max(sv) > 10^-10)
    sv <- sv[nz]
    
    V <- V[,nz]
    lX <- t(V) %*% cX
  }else{
    K <- t(cX) %*% cX
    out <- eigb((K+t(K))/2)
    V <- out$V
    S <- diag(out$D)
    
    sv <- diag(S)
    nz <- (sv/max(sv) > 10^-10)
    sv <- sv[nz]
    
    V <- V[,nz]
    dS <- diag(S[nz,nz])
    dS <- sqrt(dS)
    lX <- t(V) * dS
  }
  
  return(list(lX = lX, sv=sv, V=V))
}

kdagger <- function(D){
  num <- nrow(D)
  J <- diag(1,nrow=num) - (1/num)*(matrix(1, nrow=num, ncol=num))
  K <- -0.5 * J %*% (D - diag(diag(D))) %*% J
  return(K)
}

eigb <- function(A){
  
  out <- eigen(A)
  D <- diag(out$values)
  V <- out$vectors
  dD <- diag(D)
  
  out <- sort(dD, decreasing = TRUE, index.return=TRUE)
  dD <- out$x
  index_sort <- out$ix
  V <- V[,index_sort]
  D <- diag(D)
  
  return(list(V=V, D=D))
}

distmex <- function(qX){
  out <- (as.matrix(dist(t(qX))))^2
  return(out)
}

intersecter <- function(U1, U2, v1, v2){
  
  num <- length(v1)
  len_1 <- sum(v1)
  len_2 <- sum(v2)
  ind_1 <- which(v1 != 0)
  ind_2 <- which(v2 != 0)
  
  w1 <- matrix(rep(0,num),nrow=num)
  w1[ind_1] <- c(1:len_1)
  
  w2 <- matrix(rep(0,num),nrow=num)
  w2[ind_2] <- c(1:len_2)
  
  w <- v1 + v2
  w[w > 0] <- 1
  c <- which(v1*v2 != 0)
  k <- sum(v1*v2)
  
  #b1: atoms only in clique 1
  b1 <- pmax(v1-v2,0)
  b1 <- which(b1 != 0)
  ind_top_1 <- w1[b1]
  
  #b2: atoms only in clique 2
  b2 <- pmax(v2-v1,0)
  b2 <- which(b2 != 0)
  ind_bot_2 <- w2[b2]
  
  ind_bot_1 <- w1[c]
  ind_top_2 <- w2[c]
  
  tU1 <- rbind(U1[ind_top_1,], U1[ind_bot_1,])
  tU2 <- rbind(U2[ind_top_2,], U2[ind_bot_2,])
  
  U <- as.matrix(subspace_intersection(tU1, tU2, k)$U)
  
  raw_index <- c(b1, c, b2)
  index_sorted <- order(raw_index)
  
  U <- U[index_sorted,]
  
  return(list(U=U, w=w))
}

subspace_intersection <- function(tU1, tU2, k){
  len_1 <- nrow(tU1)
  len_2 <- nrow(tU2)
  
  U1 <- as.matrix(bdiag(tU1, diag(1, nrow=(len_2-k))))
  U2 <- as.matrix(bdiag(diag(1, nrow=len_1-k), tU2))
  
  C <- t(U1) %*% U2
  out <- svd(C, nu=nrow(C), nv = ncol(C))
  Uc <- out$u
  Sc <- out$d
  Vc <- out$v
  
  s <- Sc
  
  U <- U1 %*% Uc
  
  if(length(s) < ncol(U)){
    s <- c(s, rep(0, ncol(U)-length(s)))
  }
  
  U <- U[,s > (1 - 1e-3)]
  
  return(list(U=U, s=s))
}