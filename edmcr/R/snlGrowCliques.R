snlGrowCliques <- function(Dpartial,A,r,n,m){
  
  MaxCliqueSize <- 3*(r+1)
  
  II <- matrix(rep(0,n*MaxCliqueSize+m),nrow=n*MaxCliqueSize+m,ncol=1)
  JJ <- II
  k <- 0
  
  for(jct in 1:n){
    k <- k+1
    II[k] <- jct
    JJ[k] <- jct
    NodesToAdd <- MaxCliqueSize - 1
    
    q <- as.matrix(Dpartial[,jct] > 0)
    
    while(NodesToAdd > 0 & any(q)){
      ic <- as.matrix(which(q))
      
      k <- k+1
      II[k] <- ic[length(ic)]
      JJ[k] <- jct
      
      NodesToAdd <- NodesToAdd - 1
      q <- q & Dpartial[,ic[length(ic)]]
    }
  }
  
  if(is.null(A)){
    Dcq <- matrix(rep(0,n^2),nrow=n)
    
    for(i in 1:k){
      Dcq[II[i],JJ[i]] <- 1
    }
    
    Cp <- matrix(rep(1,n),nrow=n)
    
    eigvs <- list()
    for(i in 1:n){
      eigvs[[i]] <- NA
    }
    
  }else{
    II[seq(k+1,k+m,by=1)] <- seq(n-m+1,n,by=1)
    JJ[seq(k+1,k+m,by=1)] <- n + 1
    k <- k+m
    
    Dcq <- matrix(rep(0,n^2+n),nrow=n)
    
    for(i in 1:k){
      Dcq[II[i],JJ[i]] <- 1
    }

    Cp <- matrix(rep(1,n+1),nrow=n+1)
    
    eigvs <- list()
    for(i in 1:(n+1)){
      eigvs[[i]] <- NA
    }
  }
  
  return(list(Dcq=Dcq,Cp=Cp,eigvs=eigvs,ic=ic))
  
}