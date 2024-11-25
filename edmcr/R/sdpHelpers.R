##More helper functions for SDP

##
sdpFmerit <- function(muu,Lam,X,B,V,H,sigmaa){
  n1 <- nrow(Lam)
  Fc <- Lam %*% X - sigmaa*muu * diag(n1)
  return(Fc)
}


##
sdpFpmu <- function(dLam,dX,Lam,X,V,H){
  dLam <- Matrix(dLam, sparse=TRUE)
  dX <- Matrix(dX, sparse=TRUE)
  Lam <- Matrix(Lam, sparse=TRUE)
  X <- Matrix(X, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  
  Fd <- Matrix(2 * KWs(KW(dX,V,H),V,H) - dLam, sparse=TRUE)
  Fc <- Matrix(Lam %*% dX + dLam %*% X, sparse=TRUE)
  return(list(Fd=Fd,Fc=Fc))
}

##
sdpFpmulow <- function(Lam,X,lowinds,V,H){
  
  X <- Matrix(X, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  Lam <- Matrix(Lam, sparse=TRUE)
  
  n1 <- nrow(X)
  
  En <- Matrix(diag(n1), sparse=TRUE)
  
  Kd1 <- Matrix(kronecker(En,Lam), sparse=TRUE) %*% Matrix(lowinds, sparse=TRUE)
  Kd2 <- Matrix(kronecker(X,En), sparse=TRUE) %*% Matrix(lowinds, sparse=TRUE)
  
  Kd <- Matrix(cbind(Kd1,Kd2), sparse=TRUE) # defunct cBind
  
  return(Kd)
}

##
sdpFpmus <- function(Fd,Fc,Lam,X,V,H){
  
  Fd <- Matrix(Fd, sparse=TRUE)
  Fc <- Matrix(Fc, sparse=TRUE)
  Lam <- Matrix(Lam, sparse=TRUE)
  X <- Matrix(X, sparse=TRUE)
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  
  dX <- Matrix(2*KWs(KW(Fd,V,H),V,H)+(1/2)*(t(Fc) %*% Lam + Lam %*% Fc), sparse=TRUE)
  dLam <- Matrix(-Fd + (1/2)*(X %*% t(Fc) + Fc %*% X), sparse=TRUE)
  return(list(dX=dX,dLam=dLam))
}

##
sdpFpmuup <- function(V,H,n1){
  
  V <- Matrix(V, sparse=TRUE)
  H <- Matrix(H, sparse=TRUE)
  
  tn1 <- (n1+1)*n1/2
  n2 <- n1^2
  indn1 <- upper.tri(matrix(1,nrow=n1))
  I <- Matrix(diag(n1), sparse=TRUE)
  
  colcnt <- 0
  colcnt2 <- 0
  
  lowinds <- matrix(0,nrow=n2,ncol=tn1)
  diag(lowinds) <- 1
  lowinds <- Matrix(lowinds, sparse=TRUE)
  
  Ku <- Matrix(matrix(1,nrow=n2,ncol=tn1), sparse=TRUE)
  Ku2 <- Matrix(lowinds, sparse=TRUE)
  
  for(j in 1:n1){
    if(j > 1){
      for(i in 1:(j-1)){
        eij <- as.matrix(I[,i]) %*% t(as.matrix(I[j,]))
        eij <- Matrix(eij + t(eij), sparse=TRUE)
        temp <- Matrix(2*KWs(KW(eij,V,H),V,H), sparse=TRUE)
        colcnt <- colcnt + 1
        lowinds[,colcnt] <- eij[1:length(eij)]
        Ku[,colcnt] <- temp[1:length(temp)]
        colcnt2 <- colcnt2 + 1
        Ku2[,colcnt2] <- -eij[1:length(eij)]
      }
    }
    
    ejj <- Matrix(as.matrix(I[,j]) %*% t(as.matrix(I[j,])), sparse=TRUE)
    temp <- Matrix(2*KWs(KW(ejj,V,H),V,H), sparse=TRUE)
    colcnt <- colcnt + 1
    lowinds[,colcnt] <- ejj[1:length(ejj)]
    Ku[,colcnt] <- temp[1:length(temp)]
    colcnt2 <- colcnt2 + 1
    Ku2[,colcnt2] <- -ejj[1:length(ejj)]
  }
  
  Ku <- Matrix(Ku,sparse=TRUE)
  Ku2 <- Matrix(Ku2, sparse=TRUE)
  
  Ku <- Matrix(cbind(Ku, Ku2), sparse=TRUE)
  
  return(list(Ku = Ku, lowinds = lowinds))
  
}