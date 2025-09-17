## Kernel estimation for each individual ##
kern.est <-function(ind, beta, Xmat, Y,IDX, bw, k.type=NULL){
  p <- ncol(Xmat)


  if (is.null(k.type)) k.type = 'gaussian'
  bw.type <- 'fix.bw'

  Zi <- Xmat[ind, ]
  # Pi <- which(Xmat[ind,]!=0)
  Pi <- which(!is.na(Xmat[ind, ]))
  w <- sum(Xmat[ind,Pi]*beta[Pi])

  i.peer <-Reduce(intersect,IDX[Pi])
  Pi.peer <- which(is.na(Xmat[ind,]))
  for (j in 1:p){ # impute for each variables
    ij.peer <- intersect(i.peer,IDX[[j]])
    n_ijp <- length(ij.peer)


    # remove variable bandwidth
    # if(bw.type == 'var.bw'){# fix.bw
    #   Xbeta <-  matrix(Xmat[ij.peer, Pi], nrow=n_ijp, ncol=length(Pi)) %*% matrix(beta[Pi],nrow=length(Pi), ncol=1)
    #   Xbeta <- Xbeta[,1]
    #   Ind.knn <- order(abs(Xbeta - w))[1:K]
    #   h <-  (max(Xbeta[Ind.knn], na.rm = T) - min(Xbeta[Ind.knn], na.rm = T))/2
    #   h <- max(h, min(abs(Xmat[ij.peer,Pi]%*%beta[Pi] - w))+0.01)
    # }

    # fixed bandwidth
    if(bw.type == 'fix.bw'){
      Xbeta <-  matrix(Xmat[ij.peer, Pi], nrow=length(ij.peer), ncol=length(Pi)) %*% matrix(beta[Pi],nrow=length(Pi), ncol=1)
      Xbeta <- Xbeta[,1]
      h <- sd(Xbeta+rnorm(n_ijp,sd=1e-5)) * bw # normalize to same scale.
    }
    u <- (Xbeta - w)/(h) # ensure abs(u) < 1
    if (sum(u<=1 & u>=-1)==0 & k.type!= 'gaussian'){
      warning("Warning: the bandwidth is too small, we suggest using gaussian kernel! \n")
      Zi <- NULL
      return(Zi)
    }

    u.kern <- kern(u,type=k.type)
    Zi[j] <- sum(Xmat[ij.peer,j]*u.kern)/sum(u.kern)
  }
  return(Zi)
}
