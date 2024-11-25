#P - A Vector
#H - A Matrix
#A - A Matrix

npfHessian <- function(PVector, A, H){
  
  P <- matrix(PVector, nrow=nrow(A),byrow=FALSE)
  
  n <- nrow(P)
  r <- ncol(P)
  
  #HessianVec <- rep(0,(n*(n-1)/2+n))
  HessianVec <- rep(0,(n*r)^2)
  Index <- 1
  
  for(i in 1:n){
    for(k in 1:r){
      #j <- i
      #for(kprime in k:r){
      #  HE <- npfHEntry(P,A,H,i,j,k,kprime)
      #  HessianVec[Index] <- HE
      #  Index <- Index + 1
      #}
      #if(i+1 <= n){
      #  for(j in (i+1):n){
      for(j in 1:n){
        for(kprime in 1:r){
          HE <- npfHEntry(P,A,H,i,j,k,kprime)
          HessianVec[Index] <- HE
          Index <- Index + 1
        }
      }
    }
  }
#}
  
#Hessian[lower.tri(Hessian, diag=TRUE)] <- HessianVec
#HessianUpper <- t(Hessian)
#diag(HessianUpper) <- 0
#Hessian <- Hessian + HessianUpper

Hessian <- matrix(HessianVec,nrow=(n*r),byrow=TRUE)

return(Hessian)
  
}




#######

#P - A Vector
#H - A Matrix
#A - A Matrix
#i and k are the indices of p_{i,k}
#j and kprime are the indices of p_{j,kprime}

npfHEntry <- function(P,A,H,i,j,k,kprime){
  
  n <- nrow(P)
  r <- ncol(P)
  
  if(i == j & k == kprime){
    HEntry <- 0
    
    for(m in 1:n){
      if(m == i){
        HEntry <- HEntry + 0
      }else{
        HEntry <- HEntry + H[i,m]^2*(-2*(P[i,k]-P[m,k])^2 + (A[i,m] - sum((P[i,]-P[m,])^2)))
      }
    }
    
    HEntry <- -8*HEntry
    
  }else if(i != j & k == kprime){
    HEntry <- -8 * H[i,j]^2 * (2*(P[i,k]-P[j,k])^2 - (A[i,j] - sum((P[i,] - P[j,])^2)))
  }else if(i != j & k != kprime){
    HEntry <- -8 * H[i,j]^2 * (2 * (P[i,kprime] - P[j,kprime])*(P[i,k] - P[j,k]))
  }else if(i == j & k != kprime){
    
    HEntry <- 0
    
    for(m in 1:n){
      if(m == i){
        HEntry <- HEntry + 0
      }else{
        HEntry <- HEntry + (H[i,m]^2) * (-2*(P[i,kprime]-P[m,kprime])*(P[i,k]-P[m,k]))
      }
    }
    
    HEntry <- -8*HEntry
  }
  
  return(HEntry)
  
}