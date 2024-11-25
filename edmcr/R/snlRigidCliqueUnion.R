snlRigidCliqueUnion <- function(ic,jc,Dcq,Cp,eigvs,grow,condtolerscaling,n,Dpartial,d){
  
  flagred <- 0
  
  e22 <- Dcq[,ic] & Dcq[,jc]
  e11 <- (Dcq[,ic] - e22) > 0
  e33 <- (Dcq[,jc] - e22) > 0
  
  inds <- matrix(c(which(e11),which(e22),which(e33)),ncol=1)
  
  if(!any(e11)){
    #Clique ic is a subset of clique jc
    #Copy clique jc onto clique ic and delete clique jc
    Cp[jc] <- 0
    Dcq[,ic] <- Dcq[,jc]
    Dcq[,jc] <- 0
    if(length(eigvs) < jc){
      eigvs[[ic]] <- NA
    }else{
      eigvs[[ic]] <- eigvs[[jc]]
    }
    eigvs[[jc]] <- NA
    grow <- 1
  }else if(!any(e33)){
    #Clique jc is a subset of clique ic, so delete clique jc
    Cp[jc] <- 0
    Dcq[,jc] <- 0
    eigvs[[jc]] <- NA
    grow <- 1
  }else if(sum(e22) >= d+1){
    nvec <- c(sum(e11),sum(e22),sum(e33))

    a1 <- seq(1,sum(nvec[c(1,2)]),by=1)
    a2 <- seq(nvec[1]+1,sum(nvec),by=1)
    a1inds <- inds[a1]
    a2inds <- inds[a2]
    
    #Find Ub1
    if(length(eigvs) < ic || is.na(eigvs[[ic]])){
      Dbar <- Dpartial[a1inds,]
      Dbar <- Dbar[,a1inds]
      B <- snlKdag(Dbar)
      temp <- eigen(B)
      Ub <- temp$vectors[,order(temp$values)]
      Ub <- Ub[,seq(ncol(Ub)-d+1,ncol(Ub),by=1)]
      k <- length(a1)
      e <- matrix(rep(1,k),ncol=1)
      Ub1 <- as.matrix(cbind(Ub,e/sqrt(k)))
    }else{
      Ub1 <- eigvs[[ic]][a1inds,]
    }
    #Find Ub2
    if(length(eigvs) < jc || is.na(eigvs[[jc]])){
      Dbar <- Dpartial[a2inds,]
      Dbar <- Dbar[,a2inds]
      B <- snlKdag(Dbar)
      temp <- eigen(B)
      Ub <- temp$vectors[,order(temp$values)]
      Ub <- Ub[,seq(ncol(Ub)-d+1,ncol(Ub),by=1)]
      k <- length(a2)
      e <- matrix(rep(1,k),ncol=1)
      Ub2 <- as.matrix(cbind(Ub,e/sqrt(k)))
    }else{
      Ub2 <- eigvs[[jc]][a2inds,]
    }
    #Find U
    ############ SUBSPACE INTERSECTION #############
    temp <- snlSubspaceIntersection(nvec,Ub1,Ub2,condtolerscaling,d)
    
    U <- temp$U
    flagred <- temp$flagred
    ################################################
    if(!flagred){
      #Store U
      ii <- matrix(rep(inds,d+1),ncol=d+1)
      jj <- matrix(rep(seq(1,d+1,by=1),length(inds)),byrow=TRUE,nrow=length(inds))
      
      temp <- matrix(rep(0,n*(d+1)),nrow=n)
      
      for(k in 1:length(ii)){
        temp[ii[k],jj[k]] <- U[k]
      }
      
      eigvs[[ic]] <- temp
      
      #Update Dcq
      Cp[jc] <- 0
      Dcq[,ic] <- Dcq[,ic] | Dcq[,jc]
      Dcq[,jc] <- 0
      eigvs[[jc]] <- NA
      grow <- 1
    }
  }
  
  return(list(Dcq=Dcq,Cp=Cp,eigvs=eigvs,grow=grow,flagred=flagred))
  
}