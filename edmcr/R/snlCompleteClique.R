snlCompleteClique <- function(ic,Dcq,eigvs,Dpartial,Dcqinit,r,n,csizesinit,anchors){
  
  #Complete the EDM of the clique ic
  icinds <- which(Dcq[,ic] > 0)
  
  if(length(icinds) <= r){
    P <- NULL
    flagred <- 1
  }else{
    if(length(eigvs) < ic || is.na(eigvs[[ic]])){
      #clique ic must be a clique in the original graph
      
      p <- Dcq[,ic]
      np <- sum(p)
      
      temp <- Dpartial[which(p > 0),]
      temp <- temp[,which(p > 0)]
      
      if(length(which(temp > 0)) != np*(np-1)){
        warning("EDM not complete in completecliques")
      }
      
      D <- Dpartial[icinds,]
      D <- D[,icinds]
      B <- snlKdag(D)
      
      temp <- eigen(B)
      Ubs <- temp$vectors[,order(temp$values)]
      Ubs <- Ubs[,seq(ncol(Ubs)-r+1,ncol(Ubs),by=1)]
      
      Dbs <- diag(sort(temp$values,decreasing=FALSE))
      Dbs <- Dbs[seq(nrow(Dbs)-r+1,nrow(Dbs),1),]
      Dbs <- Dbs[,seq(ncol(Dbs)-r+1,ncol(Dbs),1)]
      
      k <- length(icinds)
      e <- matrix(rep(1,k),ncol=1)
      Ub <- cbind(Ubs, e/sqrt(k))
      
      II <- matrix(rep(icinds,r+1),ncol=r+1)
      JJ <- matrix(rep(seq(1,r+1,1),length(icinds)),nrow=length(icinds),byrow=TRUE)
      temp <- matrix(rep(0,(r+1)*n),nrow=n)
      for(i in 1:length(II)){
        temp[II[i],JJ[i]] <- Ub[i]
      }
      eigvs[[ic]] <- temp
      
      #Compute P
      P <- Ubs %*% sqrt(Dbs)
      II <- matrix(rep(icinds,r),nrow=r)
      JJ <- matrix(rep(seq(1,r,1),length(icinds)),nrow=length(icinds),byrow=TRUE)
      temp <- matrix(rep(0,(r*n)),nrow=n)
      for(i in 1:length(II)){
        temp[II[i],JJ[i]] <- P[i]
      }
      P <- temp
      flagred <- 0
      return(list(eigvs=eigvs,P=P,flagred=flagred))
    }else{
      if(is.null(anchors)){
        indsPlusAnchorClique <- icinds
      }else{
        indsPlusAnchorClique <- c(icinds,n+1)
      }
      
      temp <- sort.int(csizesinit[indsPlusAnchorClique], decreasing=TRUE,index.return=TRUE)
      temps <- temp$x
      tempi <- temp$ix
      
      inds3 <- sum(temps >= r+1)
      
      #cliques with sizes >= r+1
      if(inds3 > 0){
        cliqueinds <- t(as.matrix(indsPlusAnchorClique[tempi[seq(1,inds3,1)]]))
      }else{
        cliqueinds <- c()
      }
      
      for(im in cliqueinds){

        #determine if orig clique im is inside clique ic
        e22 <- Dcqinit[,im] & Dcq[,ic]
        if(sum(e22) == csizesinit[im]){
          #clique im is inside clique ic
          
          b <- which(Dcqinit[,im] > 0)
          mm <- length(b)
          
          #Find U and V
          
          U <- eigvs[[ic]]
          
          UTe <- as.matrix(colSums(U))
          temp <- qr(UTe)
          
          V <- qr.Q(temp,complete=TRUE)
          Rtemp <- qr.R(temp, complete=TRUE)
          
          V <- V[,seq(2,ncol(V),1)]
          UV <- U %*% V
          
          #Compute AA and B
          Jb <- diag(rep(1,mm)) - matrix(rep(1,mm^2),nrow=mm)/mm
          Ub <- U[b,]
          AA <- Jb %*% Ub %*% V
          
          temp <- Dpartial[b,]
          temp <- temp[,b]
          B <- snlKdag(temp)
          
          temp <- eigen(B)
          Vb <- temp$vectors[,order(temp$values)]
          Db <- diag(sort(temp$values, decreasing=FALSE))
          
          Vb <- Vb[,seq(ncol(Vb)-r+1,ncol(Vb),1)]
          Db <- Db[seq(nrow(Db)-r+1,nrow(Db),1),]
          Db <- Db[,seq(ncol(Db)-r+1,ncol(Db),1)]
          
          temp <- svd(Db)
          
          condB <- max(temp$d)/min(temp$d)
          
          if(condB > 50){
            P <- NULL
            flagred <- 1
            return(list(eigvs=eigvs,P=P,flagred=flagred))
          }else{
            #Compute P
            Zb <- qr.solve(AA, Vb %*% sqrt(Db))
            P <- UV[icinds,] %*% Zb
            II <- matrix(rep(icinds,r),nrow=length(icinds))
            JJ <- matrix(rep(seq(1,r,1),length(icinds)),nrow=length(icinds),byrow=TRUE)
            temp <- matrix(rep(0,(r*n)),nrow=n)

            for(i in 1:length(II)){
              temp[II[i],JJ[i]] <- P[i]
            }
            P <- temp
            flagred <- 0
            return(list(eigvs=eigvs,P=P,flagred=flagred))
          }
        }
      }
      P <- c()
      flagred <- 1
    }
  }
  
  return(list(eigvs=eigvs,P=P,flagred=flagred))
  
}