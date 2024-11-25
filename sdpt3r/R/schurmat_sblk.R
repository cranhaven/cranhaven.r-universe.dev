schurmat_sblk <- function(blk, At, par, schur, p, X, Y = NULL){
  
  iter <- par$iter
  smallblkdim <- par$smallblkdim
  
  if(length(smallblkdim) == 0){
    smallblkdim <- 50
  }
  
  if(!is.null(Y)){
    symm <- 0
  }else{
    symm <- 1
    Y <- X
  }
  
  m <- max(dim(schur))
  if(iter == 1){
    assign("nnzschur",matrix(c(rep(0, nrow(blk)-1), m^2), nrow=nrow(blk),1), pos=sys.frame(which = -3))
    assign("nzlistschur",matrix(list(),nrow(blk),1), pos=sys.frame(which = -3))
  }
  ##
  if(max(blk[[p,2]]) > smallblkdim | max(dim(as.matrix(blk[[p,2]]))) <= 10){
    ##
    ## compute schur for matrices that are sparse
    ##
    m1 <- ncol(At[[p,1]])
    if(is.null(m1)){
      m1 <- 0
    }
    if(is(schur, "sparseMAtrix")){
      schur <- as.matrix(schur)
    }
    J <- min(m1, max(which(is.finite(par$nzlistA[[p,1]])))-1)
    if(J > 0){
      if(is(X[[p]], "sparseMAtrix") & !is(Y[[p]], "sparseMAtrix")){
        X[[p]]<- as.matrix(X[[p]])
      }
      
      if(!is(X[[p]], "sparseMAtrix") & is(Y[[p]], "sparseMAtrix")){
        Y[[p]]<- as.matrix(Y[[p]])
      }
      if(iter <= 3){
        #print(blk)
        #print(p)
        #print(At[[p,1]])
        #print(par$nzlistA[[p,1]])
        #print(par$nzlistA[[p,2]])
        #print(par$permA[p,])
        #print(Y[[p]])
        #print(X[[p]])
        #print(J)
        #print(symm)
        #print(schur)
        out <- mexschur(blk, p, At[[p,1]], par$nzlistA[[p,1]], par$nzlistA[[p,2]], par$permA[p,], Y[[p]], X[[p]], J, symm, schur)
        schur <- out$schur
        tmp <- get("nnzschur", pos=sys.frame(which = -3))
        tmp[p] <- out$nzschur
        assign("nnzschur",tmp, pos=sys.frame(which = -3))
        nzlisttmp <- out$nzlist
        if(get("nnzschur", pos=sys.frame(which = -3))[p] == mexnnz(nzlisttmp)){
          nzlistschur <- get("nzlistschur", pos=sys.frame(which = -3))
          nzlistschur[[p]] <- nzlisttmp
          assign("nzlistchur",nzlistschur, pos=sys.frame(which = -3))
        }
      }else{
        if(is.null(get("nzlistschur", pos=sys.frame(which = -3))[[p]])){
          schur <- mexschur(blk, p, At[[p,1]], par$nzlistA[[p,1]], par$nzlistA[[p,2]], par$permA[p,], Y[[p]], X[[p]], J, symm, schur)$schur
        }else{
          schur <- mexschur(blk, p, At[[p,1]], par$nzlistA[[p,1]], par$nzlistA[[p,2]], par$permA[p,], Y[[p]], X[[p]], J, symm, schur, get("nzlistschur", pos=sys.frame(which = -3))[[p]])$schur
        }
      }
    }
    ##
    ## compute schur for matrices that are not so sparse or dense
    ##
    if(m1 < m){
      ss <- c(0, cumsum(blk[[p,3]]))
      len <- sum(blk[[p,3]])
      dd <- At[[p,3]]
      DD <- matrix(0, len, len)
      for(i in 1:length(dd[,2])){
        DD[dd[i,2],dd[i,3]] <- dd[i,4]
      }
      XVD <- X[[p,1]] %*% At[[p,2]] %*% DD
      YVD <- Y[[p,1]] %*% At[[p,2]] %*% DD
    }
    L <- max(which(is.finite(par$nzlistAsum[[p,1]]))) - 1
    if(J < L){
      len <- par$nzlistAsum[[p,1]][J+1]
      if(len > 0){
        list <- par$nzlistAsum[[p,2]][1:len,]
      }else{
        list <- matrix(,0,2)
      }
    }
    if(m1 > 0){
      for(k in (J+1):m){
        if(J+1 <= m){
          if(k <= m1){
            isspAk <- par$isspA[p,k]
            #Ak <- mexsmat(blk,At,isspAk,p,k)
            Ak <- mexsmat(blk,At,0,p,k)
            if(k <= L){
              idx1 <- par$nzlistAsum[[p,1]][k] + 1
              idx2 <- par$nzlistAsum[[p,1]][k+1]
              list <- rbind(list, par$nzlistAsum[[p,2]][idx1:idx2,])
              list <- list[order(list[,2], list[,1]),]
              tmp <- Prod3(blk,p,X[[p,1]],Ak,Y[[p,1]],symm,list)
            }else{
              tmp <- Prod3(blk,p,X[[p,1]],Ak,Y[[p,1]],symm)
            }
          }else{
            idx <- c((ss[k-m1]+1):ss[k-m1+1])
            tmp <- XVD[,idx] %*% t(Y[[p,1]] %*% At[[p,2]][,idx])
          }
          if(!symm){
            tmp <- 0.5*(mexsvec(blk[p,,drop=FALSE],tmp) + mexsvec(blk[p,,drop=FALSE],t(tmp)))
          }else{
            tmp <- mexsvec(blk[p,,drop=FALSE],tmp)
          }
          permk <- par$permA[p,k]
          idx <- par$permA[p,1:min(k,m1)]
          tmp2 <- schur[idx,permk] + mexinprod(blk,At,tmp,min(k,m1),p)
          schur[idx, permk] <- tmp2
          schur[permk,idx] <- t(tmp2)
        }
      }
    }
    if(m1 < m){
      m2 <- m - m1
      XVtmp <- t(XVD) %*% At[[p,2]]
      YVtmp <- t(At[[p,2]]) %*% YVD
      for(k in 1:m2){
        idx0 <- c((ss[k]+1):ss[k+1])
        tmp <- XVtmp[,idx0] * YVtmp[,idx0]
        tmp <- tmp %*% matrix(1,length(idx0),1)
        tmp3 <- schur[m1+(1:m2), m1+k] + mexqops(blk[[p,3]],tmp,matrix(1,max(dim(tmp)),1),1)
        schur[m1+(1:m2), m1+k] <- tmp3
      }
    }
  }else{
    if(is(X[[p]], "sparseMAtrix") & !is(Y[[p]], "sparseMAtrix")){
      X[[p]]<- as.matrix(X[[p]])
    }
    
    if(!is(X[[p]], "sparseMAtrix") & is(Y[[p]], "sparseMAtrix")){
      Y[[p]]<- as.matrix(Y[[p]])
    }
    tmp <- mexskron(blk,p,as.matrix(X[[p,1]]),as.matrix(Y[[p,1]]))
    schurtmp <- t(At[[p,1]]) %*% tmp %*% At[[p,1]]
    if(base::norm(par$permA[p,] - c(1:m), type="2") > 0){
      Perm <- matrix(0,m,m)
      for(i in 1:m){
        Perm[i,par$permA[p,i]] <- 1
      }
      schur <- schur + t(Perm) %*% schurtmp %*% Perm
    }else{
      schur <- schur + schurtmp
    }
  }
  return(schur)
}