checkdepconstr <- function(blk, At, b, y, rmdepconstr){
  
  m <- length(b)
  AAt <- matrix(0,m,m)
  numdencol <- 0
  UU <- c()
  
  for(p in 1:nrow(blk)){
    if(blk[[p,1]] == "s"){
      m1 <- ncol(At[[p,1]])
      if(is.null(m1)){
        m1 <- 0
      }
      m2 <- m - m1
      if(m2 > 0){
        dd <- At[[p,3]]
        lenn <- nrow(dd)
        idxD <- c(0, which(diff(dd[,1]) != 0), lenn)
        ss <- c(0,cumsum(blk[[p,3]]))
        if(length(get("existlowrank", pos=sys.frame(which = -2))) > 0 && get("existlowrank", pos=sys.frame(which = -2))){
          AAt[1:m1,1:m1] <- AAt[1:m1,1:m1] + t(At[[p,1]]) %*% At[[p,1]]
          for(k in 1:m2){
            idx <- c((ss[k]+1):ss[k+1])
            idx2 <- c((idxD[k]+1):idxD[k+1])
            ii <- dd[idx2,2] - ss[k]
            jj <- dd[idx2,3] - ss[k]
            len <- blk[[p,3]][k]
            
            Dk <- matrix(rep(0,len*len), ncol=len, nrow=len)
            for(l in 1:length(ii)){
              Dk[ii[l],jj[l]] <- dd[idx2,4][l]
            }
            tmp <- svec(blk,p,At[[p,2]][,idx] %*% Dk %*% t(At[[p,2]][,idx]))
            tmp2 <- AAt[1:m1,m1+k] + t(t(tmp) %*% At[[p,1]])
            AAt[[1:m1,m1+k]] <- tmp2
            AAt[[m1+k,1:m1]] <- t(tmp2)
          }
        }
        DD <- matrix(0, ncol=sum(blk[[p,3]]), nrow=sum(blk[[p,3]]))
        for(l in 1:length(dd[,1])){
          DD[dd[l,2],dd[l,3]] <- dd[l,4]
        }
        VtVD <- (t(At[[p,2]]) %*% At[[p,2]]) %*% DD
        VtVD2 <- t(VtVD) * VtVD
        for(k in 1:m2){
          idx0 <- c((ss[k]+1):ss[k+1])
          tmp <- VtVD2[,idx0]
          tmp <- tmp %*% matrix(1,nrow=length(idx0),ncol=1)
          tmp3 <- AAt[m1+c(1:m2),m1+k] + mexqops(blk[[p,3]],tmp,matrix(1,nrow=length(tmp),ncol=1),1)
          AAt[m1+c(1:m2),m1+k] <- tmp3
        }
      }else{
        AAt <- AAt + abs(t(At[[p,1]])) %*% abs(At[[p,1]])
      }
    }else{
      decolidx <- checkdense(t(At[[p,1]]))
      
      if(length(decolidx) > 0){
        n2 <- nrow(At[[p,1]])
        dd <- matrix(1,nrow=n2,ncol=1)
        len <- length(decolidx)
        if(len < 0.5*sum(blk[[p,2]]) || sum(blk[[p,2]]) >= 20){
          dd[decolidx] <- rep(0,len)
          AAt <- AAt + abs(t(At[[p,1]])) %*% diag(dd,nrow=n2,ncol=n2) %*% abs(At[[p,1]])
          tmp <- t(At[[p,1]][decolidx,])
          UU <- cbind(UU, tmp)
          numdencol <- numdencol + len
        }
      }else{
        if(length(which(At[[p,1]] != 0)) > 0.2*length(At[[p,1]])){
          Atp <- matrix(abs(At[[p,1]]), nrow(At[[p,1]]), ncol(At[[p,1]]))
        }else{
          Atp <- abs(At[[p,1]])
        }
        AAt <- AAt + t(Atp) %*% Atp
      }
    }
  }
  
  numdencol <- ncol(UU)
  if(is.null(numdencol)){
    numdencol <- 0
  }
  
  if(!is(AAt, 'sparseMatrix')){
    AAt <- Matrix(AAt, sparse=TRUE)
  }
  
  feasible <- 1
  neardepconstr <- 0
  assign("nnzmatold",mexnnz(AAt), pos=sys.frame(which = -2))
  rho <- 1e-15
  diagAAt <- diag(AAt)
  AAt <- mexschurfun(AAt, rho*pmax(diagAAt,1))
  
  indef <- !is.positive.definite(.5*as.matrix(AAt+t(AAt)))
  if(indef){
    idxB <- c(1:m)
    neardepconstr <- 1
    return(list(At=At, b=b, y=y, indeprows=idxB, depconstr=neardepconstr, feasible=feasible, AAt=AAt))
  }else{
    out <- Matrix::chol(AAt, pivot=TRUE)
    L.perm <- attr(out, "pivot")
    L.R <- out
    L.d <- diag(L.R)^2
  }
  
  #Find Independent rows of A
  
  dd <- matrix(0,nrow=m,ncol=1)
  idxB <- as.matrix(c(1:m))
  dd[L.perm] <- abs(L.d)
  idxN <- which(dd < 1e-13*mean(L.d))
  ddB <- dd[setdiff(1:m, idxN)]
  ddN <- dd[idxN]
  
  if(length(ddN) > 0 && length(ddB) > 0 && min(ddB)/max(ddN) < 10){
    idxN <- c()
  }
  
  if(length(idxN) > 0){
    neardepconstr <- 1
    if(numdencol == 0){
      if(rmdepconstr){
        idxB <- setdiff(1:m, idxN)
        
        out <- findcoeffsub(blk, At, idxB, idxN)
        W <- out$W
        resnorm <- out$resnorm
        tol = 1e-8
        if(resnorm > sqrt(tol)){
          idxB <- as.matrix(c(1:m))
          neardepconstr <- 0
          return(list(At=At, b=b, y=y, indeprows=idxB, depconstr=neardepconstr, feasible=feasible, AAt=AAt))
        }
        
        tmp <- t(W) %*% b[idxB] - b[idxN]
        nnorm <- base::norm(tmp,type="2")/max(1,base::norm(b,type="2"))
        if(nnorm > tol){
          feasible <- 0
        }else{
          feasible <- 1
          for(p in 1:nrow(blk)){
            At[[p,1]] <- At[[p,1]][,idxB]
          }
          b <- b[idxB]
          y <- y[idxB]
          AAt <- AAt[idxB,idxB]
        }
      }
    }
  }
  return(list(At=At, b=b, y=y, indeprows=idxB, depconstr=neardepconstr, feasible=feasible, AAt=AAt))
}