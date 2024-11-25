
mexexpand <- function(blksize,x){
  numblk <- max(nrow(as.matrix(blksize)),ncol(as.matrix(blksize)))
  cols <- sum(blksize)
  
  idx <- 1
  z <- rep(0,cols)
  
  for(k in 1:numblk){
    blkdim <- blksize[k]
    for(j in 1:blkdim){
      z[idx] <- x[k]
      idx <- idx + 1
    }
  }
  return(z)
}


mexnnz <- function(A){ 
  out <- length(which(A != 0))
  return(out)
}


mexschurfun <- function(X,Y,options=NULL){
  
  n <- nrow(X)
  Y <- as.matrix(Y)
  
  if(is.null(options)){
    if(nrow(Y) == n & ncol(Y) == n){
      options = 2
    }else{
      options = 1
    }
  }
  
  if(options == 1){
    diag(X) <- diag(X) + Y
  }else if(options == 2){
    X <- X + Y
  }
  
  return(X)
  
}


mexsvec <- function(blk,M, isspx=NULL, type=NULL){
  
  m <- nrow(M)
  n <- ncol(M)
  
  blk_cell_pr <- blk[[1,2]]
  numblk <- length(blk_cell_pr)
  blksize <- blk_cell_pr
  if(numblk == 1){
    n2 <- n*(n+1)/2
  }else{
    cumblksize <- numeric(numblk+1)
    blknnz <- numeric(numblk+1)
    n <- 0
    n2 <- 0
    for(k in 0:(numblk-1)){
      nsub <- blksize[k+1]
      n <- n + nsub
      n2 <- n2 + nsub*(nsub+1)/2
      cumblksize[k+1+1] <- n
      blknnz[k+1+1] <- n2
    }
  }
  
  #Assign Pointers
  A <- M
  isspA <- is(A, "sparseMatrix")
  iscmpA <- is.complex(A)
  
  if(isspA){
    out <- which(A != 0, arr.ind=TRUE)
    irA <- out[,1][order(out[,2],out[,1])]
    
    Atmp <- A
    Atmp[which(Atmp != 0)] <- 1
    tmp <- colSums(Atmp)
    
    jcA <- numeric(ncol(Atmp)+1)
    for(i in 1:ncol(Atmp)){
      jcA[i+1] <- jcA[i] + tmp[i]
    }
    
    NZmax <- nrow(out)
    A <- as.matrix(A[which(A != 0)])
  }else{
    NZmax <- n2
    irA <- numeric(length(which(A != 0)))
    jcA <- numeric(ncol(A)+1)
  }
  
  if(iscmpA){
    AI <- Im(A)
  }
  
  if(!is.null(isspx)){
    if(nrow(as.matrix(isspx)) > 1){
      isspB <- isspx
    }else if(NZmax < n2/2){
      isspB <- 1
    }else{
      isspB <- 0
    }
  }else{
    if(NZmax < n2/2){
      isspB <- 1
    }else{
      isspB <- 0
    }
  }
  
  if(!is.null(type)){
    type <- type
  }else{
    type <- 0
  }
  
  if(isspB){
    if(iscmpA){
      
    }else{
      B <- matrix(0,n2,1)
      irB <- numeric(NZmax)
      jcB <- numeric(ncol(B)+1)
    }
  }else{
    B <- matrix(0,n2,1)
    irB <- numeric(NZmax)
    jcB <- numeric(ncol(B)+1)
  }
  
  #Do the Computations in a subroutine
  r2 <- sqrt(2)
  if(type == 0){
    if(iscmpA){
      if(numblk == 1){
        n <- as.integer(n)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        AI <- as.matrix(AI)
        BI <- as.matrix(BI)
        out <- .C("svec1cmpWrapper",
                  n=n,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB,
                  AI=AI,
                  BI=BI)
      }else{
        n <- as.integer(n)
        numblk <- as.integer(numblk)
        cumblksize <- as.integer(cumblksize)
        blknnz <- as.integer(blknnz)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        AI <- as.matrix(AI)
        BI <- as.matrix(BI)
        out <- .C("svec2cmpWrapper",
                  n=n,
                  numblk=numblk,
                  cumblksize=cumblksize,
                  blknnz=blknnz,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB,
                  AI=AI,
                  BI=BI)
      }
    }else{
      if(numblk == 1){
        n <- as.integer(n)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        out <- .C("svec1Wrapper",
                  n=n,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB)
        B <- out[[7]]
        irB <- out[[8]]
        jcB <- out[[9]]
        isspB <- out[[10]]
        if(isspB){
          Btmp <- B
          B <- matrix(0,n2,1)
          
          for(i in 1:jcB[2]){
            B[irB[i]+1] <- Btmp[i]
          }
        }
      }else{
        n <- as.integer(n)
        numblk <- as.integer(numblk)
        cumblksize <- as.integer(cumblksize)
        blknnz <- as.integer(blknnz)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        out <- .C("svec2Wrapper",
                  n=n,
                  numblk=numblk,
                  cumblksize=cumblksize,
                  blknnz=blknnz,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB)
        B <- out[[10]]
        irB <- out[[11]]
        jcB <- out[[12]]
        isspB <- out[[13]]
        if(isspB){
          Btmp <- B
          B <- matrix(0,n2,1)
        
          for(i in 1:jcB[2]){
            B[irB[i]+1] <- Btmp[i]
          }
        }
      }
    }
  }else{
    if(iscmpA){
      if(numblk == 1){
        n <- as.integer(n)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        AI <- as.matrix(AI)
        BI <- as.matrix(BI)
        out <- .C("svec3cmpWrapper",
                  n=n,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB,
                  AI=AI,
                  BI=BI)
      }else{
        n <- as.integer(n)
        numblk <- as.integer(numblk)
        cumblksize <- as.integer(cumblksize)
        blknnz <- as.integer(blknnz)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        AI <- as.matrix(AI)
        BI <- as.matrix(BI)
        out <- .C("svec4cmpWrapper",
                  n=n,
                  numblk=numblk,
                  cumblksize=cumblksize,
                  blknnz=blknnz,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB,
                  AI=AI,
                  BI=BI)
      }
    }else{
      if(numblk == 1){
        n <- as.integer(n)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        out <- .C("svec3Wrapper",
                  n=n,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB)
        B <- out[[7]]
        irB <- out[[8]]
        jcB <- out[[9]]
        isspB <- out[[10]]
        
        if(isspB){
          Btmp <- B
          B <- matrix(0,n2,1)
          
          for(i in 1:jcB[2]){
            B[irB[i]+1] <- Btmp[i]
          }
        }
        
      }else{
        n <- as.integer(n)
        numblk <- as.integer(numblk)
        cumblksize <- as.integer(cumblksize)
        blknnz <- as.integer(blknnz)
        r2 <- as.double(r2)
        A <- as.matrix(A)
        irA <- as.integer(irA)
        jcA <- as.integer(jcA)
        isspA <- as.integer(isspA)
        B <- as.matrix(B)
        irB <- as.integer(irB)
        jcB <- as.integer(jcB)
        isspB <- as.integer(isspB)
        out <- .C("svec4Wrapper",
                  n=n,
                  numblk=numblk,
                  cumblksize=cumblksize,
                  blknnz=blknnz,
                  r2=r2,
                  A=A,
                  irA = irA,
                  jcA=jcA,
                  isspA=isspA,
                  B=B,
                  irB=irB,
                  jcB=jcB,
                  isspB=isspB)
        B <- out[[10]]
        irB <- out[[11]]
        jcB <- out[[12]]
        isspB <- out[[13]]
        
        if(isspB){
          Btmp <- B
          B <- matrix(0,n2,1)
          
          for(i in 1:jcB[2]){
            B[irB[i]+1] <- Btmp[i]
          }
        }
      }
    }
  }
  
  return(B)
}


mexqops <- function(blk,x,y,options){
  numblk <- ncol(as.matrix(blk))
  blksize <- blk
  cumblk <- rep(0,numblk+1)
  for(l in 1:numblk){
    cols <- blksize[l]
    cumblk[l+1] <- cumblk[l] + cols
  }
  n <- cumblk[numblk+1]
  
  if(options < 3){
    z <- matrix(0,nrow=numblk, ncol=1)
    numblk <- as.integer(numblk)
    cumblk <- as.integer(cumblk)
    options <- as.integer(options)
    
    z <- .C("ops1Wrapper", x, y, z, numblk, cumblk, options)[[3]]
  }else if(options >=3){
    z <- matrix(0, n, 1)
    
    numblk <- as.integer(numblk)
    cumblk <- as.integer(cumblk)
    options <- as.integer(options)
    
    z <- .C("ops3Wrapper", x, y, z, numblk, cumblk, options)[[3]]
    
  }
  
  return(z)
  
}


mexinprod <- function(blk,At,B,const=NULL,rowidx=NULL){
  
  iscellA <- is.list(At)
  mA <- nrow(At)
  if(!iscellA){
    mA <- 1
  }
  if(is.null(rowidx)){
    rowidx <- 1
  }
  
  #Assign Pointers
  
  iscellB <- is.list(B)
  isspB <- 0 #dont deal with sparse matrices
  m2 <- nrow(B)
  n2 <- ncol(B)
  
  if(isspB){
    out <- which(B != 0, arr.ind=TRUE)
    irB <- out[,1] - 1
    jcB <- out[,2] - 1
    Btmp <- B
    B <- numeric(m2)
    kstart <- jcB[1]
    kend <- jcB[2]
    for(k in kstart:kend){
      r <- irB[k+1]
      B[r+1] <- Btmp[k+1]
    }
  }else{
    B <- B
  }
  
  subs <- c(0,0)
  if(iscellA){
    subs[1] <- rowidx - 1
    subs[2] <- 0
    A <- At[[subs[1]+1,subs[2]+1]]
    m1 <- nrow(A)
    n1 <- ncol(A)
    #isspA <- is(A, "sparseMatrix")
    isspA <- 0
    if(isspA){
      out <- which(A != 0, arr.ind=TRUE)
      irA <- out[,1] - 1
      jcA <- out[,2] - 1
    }else{
      irA <- numeric(1)
      jcA <- numeric(1)
    }
  }else{
    A <- At
    m1 <- nrow(A)
    n1 <- ncol(A)
    isspA <- 0
    isspA <- 0
    if(isspA){
      out <- which(A != 0, arr.ind=TRUE)
      irA <- out[,1] - 1
      jcA <- out[,2] - 1
    }else{
      irA <- numeric(1)
      jcA <- numeric(1)
    }
  }
  
  if(is.null(const)){
    colidx <- 1
  }else{
    colidx <- const
  }
  
  tr <- numeric(colidx)
  dummy <- 0.0
  
  if(isspA){
    for(j in 0:(colidx-1)){
      A <- as.matrix(A)
      irA <- as.integer(irA)
      jcA <- as.integer(jcA)
      j <- as.integer(j)
      B <- as.matrix(B)
      dummy <- as.double(dummy)
      tr[j+1] <- .C("realdot22Wrapper",
                    A=A,
                    irA=irA,
                    jcA = jcA,
                    j=j,
                    B=B,
                    dummy=dummy)[[6]] #take -1 on indices for c indexing
    }
  }else{
    for(j in 0:(colidx-1)){
      A <- as.matrix(A)
      j <- as.integer(j)
      B <- as.matrix(B)
      m1 <- as.integer(m1)
      dummy <- as.double(dummy)
      tr[j+1] <- .C("realdot1Wrapper",
                    A=A,
                    j=j,
                    B=B,
                    m1=m1,
                    dummy=dummy)[[5]] #take -1 on indices for c indexing
    }
  }
  
  return(tr)
  
}


mexschur <- function(blk, p, Avec, nzlistA1, nzlistA2, permA, U, V, colend, type, schur,P=NULL){
  
  subs <- rep(0,2)
  nsubs <- 2
  
  subs[1] <- 0
  subs[2] <- 1
  
  blk_cell_pr <- blk[[p,2]]
  numblk <- ncol(as.matrix(blk_cell_pr))
  blksizetmp <- blk_cell_pr
  blksize <- rep(0, numblk)
  for(k in 1:numblk){
    blksize[k] <- blksizetmp[k]
  }
  ##
  #get pointers
  ##
  Avec <- Avec[which(Avec != 0)] #Can't pass through a sparse matrix to C, so just remove 0 entries
  idxstarttmp <- nzlistA1
  len <- max(dim(nzlistA1))
  idxstart <- idxstarttmp
  if(any(is.infinite(idxstart))){
    idxstart <- idxstart[-which(is.infinite(idxstart))]
  }
  #idxstart <- which(Avec != 0) - 1
  #
  #for(k in 1:len){
   idxstart[k] <- idxstarttmp[k]
  #}
  
  nzlistAtmp <- nzlistA2
  len <- nrow(nzlistA2)
  if(len > 0){
    nzlistAi <- numeric(len)
    nzlistAj <- numeric(len)
    for(k in 1:len){
      nzlistAi[k] <- nzlistAtmp[k] - 1 #Adjust indexing for C
      nzlistAj[k] <- nzlistAtmp[k + len] - 1
    }
  }else{
    nzlistAi <- numeric(0)
    nzlistAj <- numeric(0)
  }
  
  permAtmp <- permA
  m1 <- length(permA)
  permA <- permAtmp - 1
  #permA <- numeric(n1)
  #for(k in 1:m1){
   permA[k] <- permAtmp[k] - 1
  #}
  
  
  nU <- nrow(U)
  nV <- nrow(V)
  isspU <- 0
  isspV <- 0
  
  m <- nrow(schur)
  if(is.null(P)){
    existP <- 0
  }else{
    out <- which(P != 0, arr.ind=TRUE)
    irP <- out[,1] - 1
    jcP <- out[,2] - 1
    existP <- 1
  }
  
  ##
  #output
  ##
  schur <- schur 
  nzschur <- matrix(0,1,1)
  if(is.null(P)){
    calc3 <- TRUE
    nzP <- floor(0.2*m^2+5)
    P <- matrix(0,m,colend)
    jcP <- numeric(nzP)
    irP <- numeric(nzP)
  }else{
    calc3 <- 0
  }
  ##
  #compute schur
  ##
  Utmp <- double(1)
  Vtmp <- double(1)
  
  colm <- permA*m
  n <- nU
  
  if(type == 1){
    opt <- 1
  }else if(type == 0){
    opt <- 3
  }
  
  count <- 0
  schurcol <- rep(0,colend)
  for(col in 0:(colend-1)){
    if(existP){
      col <- as.integer(col)
      schurcol <- .C("setvecWrapper",
                     n=col,
                     schurcol=schurcol,
                     alpha=0.0)[[2]]
      
      if(length(jcP) > (col+1)){
        for(k in jcP[col+1]:(jcP[col+1+1]-1)){
          schurcol[irP[k+1]+1] <- 1
        }
      }else{
        k <- jcP[col+1]
        schurcol[irP[k+1]+1] <- 1
      }
    }else{
      #C Call
      col <- as.integer(col)
      schurcol <- .C("setvecWrapper",
                     n=col,
                     schurcol=schurcol,
                     alpha=1.0)[[2]]
    }
    
    if(opt == 1){
      #C call
      n <- as.integer(n)
      idxstart <- as.integer(idxstart)
      nzlistAi <- as.integer(nzlistAi)
      nzlistAj <- as.integer(nzlistAj)
      col <- as.integer(col)
      Avec <- as.matrix(Avec)
      out <- .C("schurij1Wrapper",
                n=n,
                Avec=Avec,
                idxstart=idxstart,
                nzlistAi=nzlistAi,
                nzlistAj=nzlistAj,
                U=U,
                col=col,
                schurcol=schurcol)
      schurcol <- out[[8]]
    }else if(opt == 2){
      #C call
      idxstart <- as.integer(idxstart)
      nzlistAi <- as.integer(nzlistAi)
      nzlistAj <- as.integer(nzlistAj)
      nzlistAr <- as.integer(nzlistAr)
      nzlistAc <- as.integer(nzlistAr)
      cumblksize <- as.integer(cumblksize)
      blkidx <- as.integer(blkidx)
      col <- as.integer(col)
      Avec <- as.matrix(Avec)
      out <- .C("schurij2Wrapper",
                Avec=Avec,
                idxstart=idxstart,
                nzlistAi=nzlistAi,
                nzlistAj=nzlistAj,
                Utmp=Utmp,
                nzlistAr=nzlistAr,
                nzlistAc=nzlistAc,
                cumblksize=cumblksize,
                blkidx=blkidx,
                col=col,
                schurcol=schurcol)
      schurcol <- out[[11]]
    }else if(opt == 3){
      #C call
      n <- as.integer(n)
      idxstart <- as.integer(idxstart)
      nzlistAi <- as.integer(nzlistAi)
      nzlistAj <- as.integer(nzlistAj)
      col <- as.integer(col)
      Avec <- as.matrix(Avec)
      U <- as.matrix(U)
      V <- as.matrix(V)
      out <- .C("schurij3Wrapper",
                n=n,
                Avec=Avec,
                idxstart=idxstart,
                nzlistAi=nzlistAi,
                nzlistAj=nzlistAj,
                U=U,
                V=V,
                col=col,
                schurcol=schurcol)
      schurcol <- out[[9]]
    }else if(opt == 4){
      #C call
      idxstart <- as.integer(idxstart)
      nzlistAi <- as.integer(nzlistAi)
      nzlistAj <- as.integer(nzlistAj)
      nzlistAr <- as.integer(nzlistAr)
      nzlistAc <- as.integer(nzlistAr)
      cumblksize <- as.integer(cumblksize)
      blkidx <- as.integer(blkidx)
      col <- as.integer(col)
      Avec <- as.matrix(Avec)
      out <- .C("schurij2Wrapper",
                Avec=Avec,
                idxstart=idxstart,
                nzlistAi=nzlistAi,
                nzlistAj=nzlistAj,
                Utmp=Utmp,
                Vtmp=Vtmp,
                nzlistAr=nzlistAr,
                nzlistAc=nzlistAc,
                cumblksize=cumblksize,
                blkidx=blkidx,
                col=col,
                schurcol=schurcol)
      schurcol <- out[[12]]
    }
    
    for(row in 0:col){
      if(schurcol[row+1] != 0){
        if(calc3 && count < nzP){
          jcP[col+1+1] <- count + 1
          irP[count + 1] <- row
        }
        count <- count + 1
        idx1 <- permA[row+1] + colm[col+1]
        idx2 <- permA[col + 1] + colm[row + 1]
        schur[idx1 + 1] <- schur[idx1 + 1] + schurcol[row+1]
        schur[idx2+1] <- schur[idx1 + 1]
      }
    }
  }
  
  nzschur <- count
  
  if(calc3){
    for(i in 1:length(which(P != 0))){
      P[irP[i]+1,jcP[i]+1] <- 1
    }
  }
  return(list(schur=schur, nzschur=nzschur, nzlist=P))
}


mexskron <- function(blk, p, A, B, sym=0){
  
  blk_cell_pr <- blk[[p,2]]
  numblk <- ncol(as.matrix(blk_cell_pr))
  blksizetmp <- blk_cell_pr
  blksize <- rep(0, numblk)
  for(k in 1:numblk){
    blksize[k] <- blksizetmp[k]
  }
  
  cumblksize <- rep(0,numblk+1)
  blksize2 <- rep(0,numblk+1)
  blksize4 <- rep(0,numblk+1)
  maxblksize <- 0
  for(k in 1:numblk){
    nsub <- blksize[k]
    n2 <- nsub*(nsub + 1)/2
    cumblksize[k+1] <- cumblksize[k] + nsub
    blksize2[k+1] <- blksize2[k] + n2
    blksize4[k+1] <- blksize4[k] + n2*n2
    maxblksize <- max(maxblksize, nsub)
  }
  
  isspA <- is(A, "sparseMatrix")
  
  if(isspA){
    out <- which(A != 0, arr.ind=TRUE)
    irA <- out[,1] - 1
    
    Atmp <- A
    Atmp[which(A != 0)] <- 1
    tmp <- colSums(Atmp)
    jcA <- rep(0,ncol(A)+1)
    for(i in 1:ncol(A)){
      jcA[i+1] <- jcA[i] + tmp[i]
    }
    A <- as.matrix(A[which(A != 0)])
  }else{
    irA <- numeric(length(which(A != 0)))
    jcA <- numeric(ncol(A)+1)
  }
  
  isspB <- is(B,"sparseMatrix")
  
  if(isspB){
    out <- which(B != 0, arr.ind=TRUE)
    irB <- out[,1] - 1
    
    Btmp <- B
    Btmp[which(B != 0)] <- 1
    tmp <- colSums(Btmp)
    jcB <- rep(0,ncol(B)+1)
    for(i in 1:ncol(B)){
      jcB[i+1] <- jcB[i] + tmp[i]
    }
    B <- as.matrix(B[which(B != 0)])
  }else{
    irB <- numeric(length(which(B != 0)))
    jcB <- numeric(ncol(B)+1)
  }
  
  P <- matrix(0, maxblksize, maxblksize)
  Q <- matrix(0, maxblksize, maxblksize)
  vvtmp <- matrix(0, maxblksize*(maxblksize + 1)/2,1)
  
  x1 <- double(maxblksize)
  y1 <- double(maxblksize)
  x2 <- double(maxblksize)
  y2 <- double(maxblksize)
  
  ##
  #Create output argument
  ##
  len <- blksize4[numblk+1]
  V <- matrix(0, len, 1)
  irV <- numeric(len)
  jcV <- numeric(blksize2[numblk+1]+1)
  ##
  #Do computations
  ##
  for(l in 0:(numblk-1)){
    blklen <- blksize[l+1]
    for(j in 0:(blklen-1)){
      jn <- j*maxblksize
      for(k in 0:(blklen-1)){
        idx <- k + jn
        P[idx+1] <- 0
        Q[idx+1] <- 0
      }
    }
    idxstart <- cumblksize[l+1] #Adjust indexing
    idxend <- cumblksize[l+1+1]
    
    if(isspA & isspB){
      for(j in idxstart:(idxend-1)){
        kstart <- jcA[j+1]
        kend <- jcA[j + 1 + 1]
        jn <- (j - idxstart)*maxblksize
        for(k in kstart:(kend-1)){
          idx <- irA[k+1] - idxstart
          P[idx + jn + 1] <- A[k+1]
        }
        kstart <- jcB[j+1]
        kend <- jcB[j+1+1]
        for(k in kstart:(kend-1)){
          idx <- irB[k+1] - idxstart
          Q[idx + jn + 1] <- B[k+1]
        }
      }
    }else{
      for(j in idxstart:(idxend-1)){
        colidx <- j*cumblksize[numblk+1]
        jn <- (j - idxstart)*maxblksize
        for(k in idxstart:(idxend-1)){
          idx <- (k-idxstart) + jn
          P[idx + 1] <- A[k + colidx + 1]
          Q[idx + 1] <- B[k + colidx + 1]
        }
      }
    }
    ##
    #SKRON
    ##
    blklen2 <- blklen*(blklen+1)/2
    idx <- blksize4[l+1]
    for(j in 0:(blklen-1)){
      for(i in 0:j){
        if(sym == 0){
          #C Call
          blklen <- as.integer(blklen)
          maxblksize <- as.integer(maxblksize)
          P <- as.matrix(P)
          Q <- as.matrix(Q)
          x1 <- as.double(x1)
          y1 <- as.double(y1)
          x2 <- as.double(x2)
          y2 <- as.double(y2)
          i <- as.integer(i)
          j <- as.integer(j)
          vvtmp <- as.matrix(vvtmp)
          
          out <- .C("skronWrapper",
                   n = blklen,
                   maxblksize = maxblksize,
                   P = P,
                   Q = Q,
                   x1 = x1,
                   y1 = y1,
                   x2 = x2,
                   y2 = y2,
                   r = i,
                   c = j,
                   vvtmp = vvtmp)
          
          blklen <- out[[1]]
          maxblksize <- out[[2]]
          P <- out[[3]]
          Q <- out[[4]]
          x1 <- out[[5]]
          y1 <- out[[6]]
          x2 <- out[[7]]
          y2 <- out[[8]]
          i <- out[[9]]
          j <- out[[10]]
          vvtmp <- out[[11]]
                    
        }else{
          #C call
          blklen <- as.integer(blklen)
          maxblksize <- as.integer(maxblksize)
          P <- as.matrix(P)
          Q <- as.matrix(Q)
          x1 <- as.double(x1)
          y1 <- as.double(y1)
          x2 <- as.double(x2)
          y2 <- as.double(y2)
          i <- as.integer(i)
          j <- as.integer(j)
          vvtmp <- as.matrix(vvtmp)
          
          out <- .C("skron2Wrapper",
                    n = blklen,
                    maxblksize = maxblksize,
                    P = P,
                    Q = Q,
                    x1 = x1,
                    y1 = y1,
                    x2 = x2,
                    y2 = y2,
                    r = i,
                    c = j,
                    vvtmp = vvtmp)
          
          blklen <- out[[1]]
          maxblksize <- out[[2]]
          P <- out[[3]]
          Q <- out[[4]]
          x1 <- out[[5]]
          y1 <- out[[6]]
          x2 <- out[[7]]
          y2 <- out[[8]]
          i <- out[[9]]
          j <- out[[10]]
          vvtmp <- out[[11]]
        }
        
        colidx <- blksize2[l+1] + i + j*(j+1)/2
        rowidx <- blksize2[l+1]
        for(k in 0:(blklen2-1)){
          V[idx+1] <- vvtmp[k + 1]
          irV[idx+1] <- rowidx+k
          idx <- idx + 1
        }
        jcV[colidx + 1 + 1] <- idx
      }
    }
  }
  
  Vtmp <- V
  V <- Matrix(0,blksize2[numblk+1], blksize2[numblk+1])
  jcVdiff <- diff(jcV)
  count <- 0
  for(i in 1:length(jcVdiff)){
    for(j in 1:jcVdiff[i]){
      V[irV[count + j]+1,i] <- Vtmp[count+j]
    }
    count <- count + jcVdiff[i]
  }
  
  return(V)
}


mexMatvec <- function(A, y, options=0){
  
  m1 <- nrow(A)
  n1 <- ncol(A)
  isspA <- 0
  
  m2 <- length(y)
  n2 <- 1
  
  if(options == 0){
    Ay <- matrix(0,m1,1)
  }else{
    Ay <- matrix(0,n1,1)
  }
  ##
  #main body
  ##
  if(options == 0){
    for(j in 0:(n1-1)){
      jm1 <- j*m1
      tmp <- y[j+1]
      if(tmp != 0){
        #C Call
        A <- as.double(A)
        jm1 <- as.integer(jm1)
        tmp1 <- as.integer(0)
        m1 <- as.integer(m1)
        tmp <- as.double(tmp)
        Ay <- as.double(Ay)
        tmp2 <- as.integer(0)
        Ay <- .C("saxpymatWrapper",A,jm1,tmp1,m1,tmp,Ay,tmp2)[[6]]
      }
    }
  }else{
    for(j in 0:(n1-1)){
      jm1 <- j*m1
      #C Call
      A <- as.double(A)
      jm1 <- as.integer(jm1)
      y <- as.double(y)
      m1 <- as.integer(m1)
      r <- as.double(0)
      Ay[j+1] <- .C("realdotdeWrapper", A, jm1, y, m1, r)[[5]]
    }
  }
  return(Ay)
}


mexProd2nz <- function(blk,p,A,B,list){
  
  m1 <- nrow(A)
  n1 <- ncol(A)
  isspA <- 0
  irA <- numeric(0)
  jcA <- numeric(0)
  
  m2 <- nrow(B)
  n2 <- ncol(B)
  isspB <- 0
  
  mlist <- nrow(list)
  nlist <- ncol(list)
  
  listtmp <- list
  list1 <- matrix(0,mlist,1)
  list2 <- matrix(0,mlist,1)
  
  for(k in 0:(mlist-1)){
    list1[k+1] <- listtmp[k+1] - 1
    list2[k+1] <- listtmp[mlist+k+1] - 1
  }
  
  P <- numeric(mlist)
  irP <- numeric(mlist)
  jcP <- numeric(n2+1)
  
  #C call
  
  n1 <- as.integer(n1)
  m2 <- as.integer(m2)
  n2 <- as.integer(n2)
  irA <- as.integer(irA)
  jcA <- as.integer(jcA)
  isspA <- as.integer(isspA)
  irP <- as.integer(irP)
  jcP <- as.integer(jcP)
  list1 <- as.integer(list1)
  list2 <- as.integer(list2)
  mlist <- as.integer(mlist)
  
  A <- as.matrix(A)
  B <- as.matrix(B)
  P <- as.matrix(P)
  
  out <- .C("prod1Wrapper",
            m=n1,
            n=m2,
            p=n2,
            A=A,
            irA=irA,
            jcA=jcA,
            isspA=isspA,
            B=B,
            P=P,
            irP=irP,
            jcP=jcP,
            list1=list1,
            list2=list2,
            len=mlist)
  Ptmp <- out[[9]]
  irP <- out[[10]]
  jcP <- out[[11]]
  
  P <- matrix(0,n1,n2)
  count <- 0
  jcPdiff <- diff(jcP)
  for(i in 1:length(jcPdiff)){
    if(jcPdiff[i] > 0){
      irPnewrow <- irP[c((count+1):(count+jcPdiff[i]))]
      Pnewrow <- Ptmp[c((count+1):(count+jcPdiff[i]))]
      for(j in 1:length(irPnewrow)){
        P[irPnewrow[j]+1,i] <- Pnewrow[j]
      }
      count <- count + jcPdiff[i]
    }
  }
  
  return(P)
}


mexsmat <- function(blk,x,isspM=NULL,rowidx=1,colidx=NULL){
  ir2 <- 1/sqrt(2)
  
  iscellA <- is.list(x)
  if(iscellA){
    mA <- nrow(x)
    nA <- ncol(x)
  }else{
    mA <- 1
    nA <- 1
  }
  ##
  #main body
  ##
  blk_cell_pr <- blk[[rowidx,2]]
  numblk <- length(blk_cell_pr)
  blksize <- blk_cell_pr
  cumblksize <- rep(0,numblk+1)
  blknnz <- rep(0,numblk+1)
  n <- 0
  n2 <- 0
  for(k in 0:(numblk-1)){
    nsub <- blksize[k+1]
    n <- n + nsub
    n2 <- n2 + nsub*(nsub+1)/2
    cumblksize[k+1+1] <- n
    blknnz[k+1+1] <- n2
  }
  ##
  ##Assign Pointers
  ##
  if(iscellA){
    A_cell_pr <- as.matrix(x[[rowidx, 1]])
    A <- A_cell_pr
    m1 <- nrow(A_cell_pr)
    n1 <- ncol(A_cell_pr)
    isspA <- is(A, "sparseMatrix")
    iscmpA <- is.complex(A)
    if(isspA){
      out <- which(A != 0, arr.ind=TRUE)
      irA <- out[,1] - 1
      
      jcA <- rep(0, ncol(A)+1)
      A[which(A != 0)] <- 1
      tmpA <- colSums(A)
      for(i in 1:length(tmpA)){
        jcA[i+1] <- jcA[i] + tmpA[i]
      }
      
    }else{
      irA <- numeric(0)
      jcA <- numeric(0)
    }
    if(iscmpA){
      AI <- which(is.complex(A), arr.ind=TRUE)
    }
  }else{
    A <- x
    m1 <- nrow(A)
    n1 <- ncol(A)
    isspA <- is(A, "sparseMatrix")
    iscmpA <- is.complex(A)
    if(isspA){
      out <- which(A != 0, arr.ind=TRUE)
      irA <- out[,1] - 1
      jcA <- rep(0, ncol(A)+1)
      A[which(A != 0)] <- 1
      tmpA <- colSums(A)
      for(i in 1:length(tmpA)){
        jcA[i+1] <- jcA[i] + tmpA[i]
      }
    }else{
      irA <- numeric(length(which(A != 0)))
      if(!is.null(n1)){
        jcA <- numeric(n1 + 1)
      }else{
        jcA <- numeric(2)
      }
    }
    if(iscmpA){
      AI <- which(is.complex(A), arr.ind=TRUE)
    }
  }
  
  if(numblk > 1){
    isspB <- 1
  }else{
    if(!is.null(isspM)){
      isspB <- isspM
    }else{
      isspB <- isspA
    }
  }
  
  if(!is.null(colidx)){
    colidx <- colidx - 1
  }else{
    colidx <- 0
  }
  ##
  #create return argument
  ##
  if(isspB){
    if(isspA){
      NZmax <- jcA[colidx+1+1] - jcA[colidx+1]
    }else{
      NZmax <- blknnz[numblk+1]
    }
    if(iscmpA){
      B <- numeric(2*NZmax)
    }else{
      B <- numeric(2*NZmax)
    }
    irB <- numeric(2*NZmax)
    jcB <- numeric(n+1)
    if(iscmpA){
      BI <- which(is.complex(A), arr.ind=TRUE)
    }
  }else{
    if(iscmpA){
      B <- matrix(0,n,n)
    }else{
      B <- matrix(0,n,n)
    }
    irB <- numeric(0)
    jcB <- numeric(0)
    if(iscmpA){
      BI <- which(is.complex(A), arr.ind=TRUE)
    }
  }
  ##
  #Do the computations in a subroutine
  ##
  
  if(iscmpA){
    
  }else{
    if(numblk == 1){
      n <- as.integer(n)
      ir2 <- as.double(ir2)
      A <- as.matrix(A)
      irA <- as.integer(irA)
      jcA <- as.integer(jcA)
      isspA <- as.integer(isspA)
      m1 <- as.integer(m1)
      colidx <- as.integer(colidx)
      B <- as.matrix(B)
      irB <- as.integer(irB)
      jcB <- as.integer(jcB)
      isspB <- as.integer(isspB)

      #C Call
      out <- .C("smat1Wrapper",
                n=n, 
                ir2=ir2,
                A=A, 
                irA=irA,
                jcA=jcA,
                isspA = isspA,
                mA=m1,
                colidx=colidx,
                B=B,
                irB=irB,
                jcB = jcB,
                isspB = isspB)
      
      B <- out[[9]]
      irB <- out[[10]] + 1
      jcB <- out[[11]]
      isspB <- out[[12]]
      
      if(isspB){
        Btmp <- B
        B <- matrix(0,n2,1)
        
        for(i in 1:jcB[length(jcB)]){
          B[irB[i]+1] <- Btmp[i]
        }
      }
      
    }else{
      n <- as.integer(n)
      numblk <- as.integer(numblk)
      cumblksize <- as.integer(cumblksize)
      blknnz <- as.integer(blknnz)
      ir2 <- as.double(ir2)
      A <- as.matrix(A)
      irA <- as.integer(irA)
      jcA <- as.integer(jcA)
      isspA <- as.integer(isspA)
      m1 <- as.integer(m1)
      colidx <- as.integer(colidx)
      B <- as.matrix(B)
      irB <- as.integer(irB)
      jcB <- as.integer(jcB)
      isspB <- as.integer(isspB)
      #C call
      out <- .C("smat2Wrapper",
                n=n,
                numblk=numblk,
                cumblksize=cumblksize,
                blknnz = blknnz,
                ir2 = ir2,
                A=A,
                irA = irA,
                jcA = jcA,
                isspA = isspA,
                mA = m1,
                colidx = colidx,
                B=B,
                irB = irB,
                jcB = jcB,
                isspB = isspB)
      
      B <- out[[12]]
      irB <- out[[13]]
      jcB <- out[[14]]
      isspB <- out[[15]]
      
      if(isspB){
        Btmp <- B
        B <- matrix(0,n,n)
        count <- 0
        for(i in 1:n){
          numinthisrow <- jcB[i+1] - jcB[i]
          if(numinthisrow > 0){
            B[irB[count + seq(1,numinthisrow,1)]+1,i] <- Btmp[count + seq(1,numinthisrow,1)]
          }
          count <- count + numinthisrow
        }
      }
    }
  }
  
  if(isspB){
    B <- B + t(B)
  }
  
  return(B)
}


mextriang <- function(U,b,options=1){
  n <- nrow(U)
  y <- matrix(0,nrow=n,ncol=1)
  if(options == 1){
    for(k in 0:(n-1)){
      y[k+1] <- b[k+1]
    }
    #C call
    y <- as.double(y)
    #U <- as.double(U)
    n <- as.integer(n)
    
    out <- .C("bwsolveWrapper",y,U,n)[[1]] #want y to be returned after modification
  }else if(options == 2){
    #C Call
    #y <- as.double(y)
    #U <- as.double(U)
    #b <- as.double(b)
    n <- as.integer(n)
    
    out <- .C("fwsolveWrapper",y,U,b,n)[[1]] #want y to be returned after modification
  }
  return(out)
}

mextriangsp <- function(U,b,options=1){
  
  n <- nrow(U)
  isspU <- 1

  out <- which(U != 0, arr.ind = TRUE)
  irU <- out[,1]
  jcU <- out[,2]
  x <- matrix(0,n,1)
  if(options == 1){
    out <- .C("bwsolve2",n,U,irU,jcU,b,x)
  }else if(options == 2){
    out <- .C("fwsolve2",n,U,irU,jcU,b,x)
  }
  
  return(x)
  
}

