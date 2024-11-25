snlRigidNodeAbsorption <- function(ic,jc,Dpartial,Dcq,eigvs,grow,Dcqinit,condtolerscaling,r,n,csizesinit){
  flagred <- 0
  
  e22 <- Dpartial[,jc] & Dcq[,ic]
  ne22 <- sum(e22)
  
  if(ne22 == sum(Dcq[,ic]) & (length(eigvs) < ic || is.na(eigvs[[ic]]))){
    Dcq[jc,ic] <- 1
    grow <- 1
  }else{
    temp <- Dpartial[e22,]
    temp <- temp[,e22]
    IntersectionComplete <- (sum(temp == 0)) == ne22*(ne22-1)
    
    #Complete clique ic if necessary
    if(!IntersectionComplete){
      
      ########## COMPLETE CLIQUE ##########
    
      temp <- snlCompleteClique(ic,Dcq,eigvs,Dpartial,Dcqinit,r,n,csizesinit)
      
      eigvs <- temp$eigvs
      P <- temp$P
      flagred <- temp$flagred
      
      #####################################
      
    }
    
    #If complete clique was successful, perform node absorption
    if(!flagred){
      e11 <- (Dcq[,ic] - e22) > 0
      e33 <- matrix(rep(0,n),ncol=1)
      
      for(i in jc){
        e33[i] <- 1
      }
      
      inds <- matrix(c(which(e11 > 0),which(e22 > 0),which(e33 > 0)),ncol=1)
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
        Ub <- Ub[,seq(ncol(Ub)-r+1,ncol(Ub),by=1)]
        k <- length(a1)
        e <- matrix(rep(1,k),ncol=1)
        Ub1 <- as.matrix(cbind(Ub,e/sqrt(k)))
      }else{
        Ub1 <- eigvs[[ic]][a1inds,]
      }
      
      #Find Ub2
      if(IntersectionComplete){
        temp <- Dpartial[a2inds,]
        temp <- temp[,a2inds]
        B <- snlKdag(temp)
      }else{
        v <- matrix(Dpartial[,jc], ncol=length(jc))
        v <- matrix(v[c(e22),], ncol=length(jc))
        temp <- cbind(snlK(as.matrix(P[e22,] %*% t(P[e22,]))),v)
        B <- snlKdag(as.matrix(rbind(temp, cbind(t(v),0))))
      }
      
      temp <- eigen(B)
      Ub <- temp$vectors[,order(temp$values)]
      Ub <- Ub[,seq(ncol(Ub)-r+1,ncol(Ub),by=1)]
      k <- length(a2)
      e <- matrix(rep(1,k),ncol=1)
      Ub2 <- as.matrix(cbind(Ub,e/sqrt(k)))
      
      #Find U
      
      ############# SUBSPACE INTERSECTION ############
      temp <- snlSubspaceIntersection(nvec,Ub1,Ub2,condtolerscaling)
      
      U <- temp$U
      flagred <- temp$flagred
      #################################################
      
      if(!flagred){
        #Store U
        ii <- matrix(rep(inds,r+1),ncol=r+1)
        jj <- matrix(rep(seq(1,r+1,by=1),length(inds)),byrow=TRUE,nrow=length(inds))
        
        temp <- matrix(rep(0,n*(r+1)),nrow=n)
        
        for(k in 1:length(ii)){
          temp[ii[k],jj[k]] <- U[k]
        }
        
        eigvs[[ic]] <- temp
        
        #Update Dcq
        Dcq[jc,ic] <- 1
        grow <- 1
      }
    }
  }
  
  return(list(Dcq=Dcq,eigvs=eigvs,grow=grow,flagred=flagred))
  
}