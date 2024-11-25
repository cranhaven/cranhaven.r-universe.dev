infeaspt <- function(blk,At,C,b,options=NULL,scalefac=NULL){
  
  if(is.null(options)){
    options <- 1
  }
  if(options == 1){
    scalefac <- c()
  }
  if(options == 2 & is.null(scalefac)){
    scalefac <- 1000
  }
  
  ##
  
  m <- length(b)
  if(all(dim(At) == c(nrow(blk),m))){
    convertyes <- rep(0, nrow(blk))
    for(p in 1:nrow(blk)){
      if(blk[[p,1]] == "s" & all(dim(At[[p,1]]) == sum(blk[[p,2]]))){
        convertyes[p] <- 1
      }
    }
    if(any(convertyes)){
      At <- svec(blk,At,rep(1,nrow(blk)))
    }
  }
  
  ##
  
  X0 <- matrix(list(), nrow=nrow(C), ncol=ncol(C))
  Z0 <- matrix(list(), nrow=nrow(C), ncol=ncol(C))
  m <- length(b)
  for(p in 1:nrow(blk)){
    blktmp <- blk[[p,2]]
    n <- max(dim(C[[p]]))
    y0 <- rep(0, m)
    b2 <- 1 + abs(t(b))
    if(options == 1){
      if(blk[[p,1]] == "s"){
        normAni <- c()
        X0[[p,1]] <- matrix(0,nrow=n,ncol=n)
        Z0[[p,1]] <- matrix(0,nrow=n,ncol=n)
        ss <- c(0, cumsum(blktmp))
        tt <- c(0, cumsum(blktmp*(blktmp+1)/2))
        for(i in 1:max(dim(as.matrix(blk[[p,2]])))){
          if(length(At[[p,1]]) > 0){
            pos <- c((tt[i]+1):tt[i+1])
            Ai <- At[[p,1]][pos,]
            normAni <- 1 + sqrt(colSums(Ai*Ai))
          }
          if(ncol(At) >= 2){
            dd <- At[[p,3]]
            qq <- c(0, cumsum(blk[[p,3]]))
            normtmp <- rep(1,max(dim(blk[[p,3]])))
            idxD <- c(0, which(diff(dd[,1]) != 0), nrow(dd))
            for(k in 1:max(dim(blk[[p,3]]))){
              idx <- c((qq[k]+1):qq[k+1])
              idx2 <- c((idxD[k]+1):idxD[k+1])
              Ak <- At[[p,2]][,idx]
              ii <- dd[idx2,2] - qq[k]
              jj <- dd[idx2,3] - qq[k]
              len <- blk[[p,3]][k]
              
              Dk <- matrix(rep(0,len*len), ncol=len, nrow=len)
              for(l in 1:length(ii)){
                Dk[ii[l],jj[l]] <- dd[idx2,4][l]
              }
              
              tmp <- t(Ak) %*% Ak %*% Dk
              normtmp[k] <- 1 + sqrt(sum(tmp * tmp))
            }
            normAni <- c(normAni, normtmp)
          }
          
          pos <- c((ss[i]+1):ss[i+1])
          ni <- length(pos)
          tmp <- C[[p,1]][pos,pos]
          normCni <- as.numeric(1 + sqrt(sum(tmp*tmp)))
          const <- 10
          constX <- max(const,sqrt(ni), ni*as.matrix(b2)/normAni)
          constZ <- max(c(const,sqrt(ni), normAni, normCni))
          X0[[p,1]][pos,pos] <- constX*diag(1+1e-10*c(randmat(ni,1,0,"u")),nrow=ni) 
          Z0[[p,1]][pos,pos] <- constZ*diag(1+1e-10*c(randmat(ni,1,0,"u")),nrow=ni)
        }
      }else if(blk[[p,1]] == "q"){
        s <- 1 + c(0, cumsum(blktmp))
        len <- max(dim(as.matrix(blktmp)))
        normC <- 1 + base::norm(C[[p,1]], type="2")
        normA <- 1 + sqrt(colSums(At[[p,1]] * At[[p,1]]))
        idenqX <- matrix(0,sum(blktmp),1)
        idenqZ <- matrix(0,sum(blktmp),1)
        idenqX[s[1:len]] <- max(1,as.matrix(b2)/normA)*sqrt(blktmp)
        idenqZ[s[1:len]] <- max(sqrt(blktmp), max(normA,normC)) * rep(1,len)
        idenqX[s[1:len]] <- idenqX[s[1:len]] * (1+1e-10*randmat(len,1,0,"u"))
        idenqZ[s[1:len]] <- idenqZ[s[1:len]] * (1+1e-10*randmat(len,1,0,"u"))
        X0[[p,1]] <- idenqX
        Z0[[p,1]] <- idenqZ
      }else if(blk[[p,1]] == "l"){
        normC <- 1 + base::norm(C[[p,1]], type="2")
        normA <- 1 + sqrt(colSums(At[[p,1]] * At[[p,1]])) 
        const <- 10
        constX <- max(const,sqrt(n), sqrt(n)*b2/normA)
        constZ <- max(const,sqrt(n), normA, normC)
        X0[[p,1]] <- constX*(1+1e-10*randmat(n,1,0,"u")) 
        Z0[[p,1]] <- constZ*(1+1e-10*randmat(n,1,0,"u"))
      }else if(blk[[p,1]] == "u"){
        X0[[p,1]] <- matrix(0,nrow=n,ncol=1)
        Z0[[p,1]] <- matrix(0,nrow=n,ncol=1)
      }else{
        stop("blk not specified correctly")
      }
    }else if(options == 2){
      if(blk[[p,1]] == "s"){
        n <- sum(blktmp)
        X0[[p,1]] <- scalefac*diag(1+1e-10*randmat(n,1,0,"u"),nrow=n,ncol=n)
        Z0[[p,1]] <- scalefac*diag(1+1e-10*randmat(n,1,0,"u"),nrow=n,ncol=n)
      }else if(blk[[p,1]] == "q"){
        s <- 1 + c(0, colSums(as.matrix(blktmp)))
        len <- max(dim(as.matrix(blktmp)))
        idenq <- rep(0,sum(blktmp))
        idenq[s[1:len]] <- 1+1e-10*randmat(len,1,0,"u")
        X0[[p,1]] <- scalefac*idenq
        Z0[[p,1]] <- scalefac*idenq
      }else if(blk[[p,1]] == "l"){
        X0[[p,1]] <- scalefac*(1+1e-10*randmat(n,1,0,"u"))
        Z0[[p,1]] <- scalefac*(1+1e-10*randmat(n,1,0,"u"))
      }else if(blk[[p,1]] == "u"){
        X0[[p,1]] <- matrix(0,nrow=n,ncol=1)
        Z0[[p,1]] <- matrix(0,nrow=n,ncol=1)
      }else{
        stop("blk not specified correctly")
      }
    }
  }
  
  return(list(X0=X0,y0=y0,Z0=Z0))
  
}