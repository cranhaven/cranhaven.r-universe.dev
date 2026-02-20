
########################################
########################################

## All pooling configs for H3
S3.configs <- function(psz){
  if(max(psz) < 4) stop("The maximum master pool size must be 4 or bigger")
  psz <- psz[psz >= 4]
  pmat <- NULL
  for(u in psz){
    pmat <- rbind(pmat, cbind(u, 2:(u-1)))
  }
  res <- NULL
  for(i in 1:nrow(pmat)){
    cz <- pmat[i, ]
    if( (cz[1] %% cz[2]) == 0 ){
      res <- rbind(res, c(cz,1))
    }
  }
  if(is.null(res)) stop("Pool size at any stage must be divisible by the next-stage pool size")
  return( res )
}

## All pooling configs for H4
S4.configs <- function(psz){
  if(max(psz) < 8) stop("The maximum master pool size must be 8 or bigger")
  psz <- psz[psz >= 8]
  rs <- NULL
  for(u in psz){
    cs2 <- 4:(u-1)
    res1 <- NULL
    for(v in cs2){
      res1 <- rbind(res1, cbind(v, 2:(v-1)))
    }
    rs <- rbind(rs, cbind(u, res1))
  }
  res <- NULL
  for(i in 1:nrow(rs)){
    cz <- rs[i, ]
    if( (cz[1] %% cz[2])==0 && (cz[2] %% cz[3])==0 ){
      res <- rbind(res, c(cz, 1))  
	}
  }
  if(is.null(res)) stop("Pool size at any stage must be divisible by the next-stage pool size")
  return( res )
}

## Simulating hierarchical testing data
hier.gt.sim <- function(N,p=0.10,S,psz,Se,Sp,assayID,Yt=NULL){
  c.s <- psz        ## Pool sizes
  S <- length(c.s)  ## Number of stages
  if(min(c.s)<=0) stop("Pool size cannot be negative or zero")
  if(S > 1){
    quot <- rep(-9,S-1)
    for(s in 1:(S-1)){quot[s] <- c.s[s]%%c.s[s+1]}
    if(max(quot) > 0) stop("Pool size at any stage must be divisible by the next-stage pool size")
  }
  ## Setting up pooling configurations
  M <- floor(N/c.s[1])
  N0 <- M*c.s[1]
  if( !is.null(Yt) ){
    Ytil1 <- Yt 
  }else{
    Ytil1 <- stats::rbinom(N,1,p)
  }
  Rem <- N-N0
  Psz <- n.div <- list()
  Psz[[1]] <- rep(c.s[1],M)
  n.div[[1]] <- rep(1,length(Psz[[1]]))
  n.sm <- matrix(-9,length(Psz[[1]]),S)
  n.sm[ ,1] <- 1
  if(S > 1){
    for( s in 1:(S-1) ){
      store <- tmp <- NULL
      for(i in 1:length(Psz[[s]])){
        temp <- rep( c.s[s+1],floor(Psz[[s]][i]/c.s[s+1]) )
        store <- c(store,temp)
        tmp <- c(tmp,length(temp))
      }
      Psz[[s+1]] <- store
      n.div[[s+1]] <- tmp
    }
    vec <- rep(1,length(Psz[[1]]))
    for(s in 1:(S-1) ){
      id0 <- cumsum(c(0,vec))
      for(i in 1:length(Psz[[1]])){
        vec[i] <- sum(n.div[[s+1]][(id0[i]+1):id0[i+1]])
      }
      n.sm[ ,s+1] <- vec
    }
  }
  Zmat <- NULL
  cc <- cumsum(c(0,colSums(n.sm)))
  id <- cumsum(c(0,Psz[[1]]))
  pl.res <- rep(-9,cc[S+1])
  ## Simulating pooled responses at stage 1
  for(m in 1:M){
    mid <- (id[m]+1):id[m+1]
    prob1 <- ifelse(sum(Ytil1[mid])>0,Se[1],1-Sp[1])
    z1 <- stats::rbinom(1,1,prob1)
    pl.res[m] <- z1
    Zmat <- rbind(Zmat,c(z1,length(mid),Se[1],Sp[1],assayID[1],mid))
  }
  if(S == 1){
    if(Rem > 0){
      rid1 <- (N0+1):N
      zr1 <- rbinom(1,1,ifelse(sum(Ytil1[rid1])>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,c(zr1,Rem,Se[1],Sp[1],assayID[1],rid1,rep(-9,c.s[1]-Rem)))
    }
  }
  ## Simulating pooled responses from subsequent stages 
  if( S > 1){
    for(s in 2:S){
      Z1 <- pl.res[(cc[s-1]+1):cc[s]]
      cid <- cumsum(c(0,Psz[[s]]))
      cn <- cumsum(c(0,n.div[[s]]))
      tmp1 <- NULL
      for(d in 1:length(Psz[[s-1]])){
        tmp3 <- NULL
        if(Z1[d]==0){
          tmp3 <- rep(0,length((cn[d]+1):cn[d+1]))
        }
        if(Psz[[s-1]][d]==1){
          tmp3 <- Z1[d]
        }
        if(Psz[[s-1]][d]>1){
          if(Z1[d] == 1){
            for(i in (cn[d]+1):cn[d+1]){
              crng <- (cid[i]+1):cid[i+1]
              probs <- ifelse(sum(Ytil1[crng])>0,Se[s],1-Sp[s])
              ztp1 <- stats::rbinom(1,1,probs)
              tmp3 <- c(tmp3,ztp1)
              fill1 <- rep(-9,Psz[[1]][1]-length(crng))
              Zmat <- rbind(Zmat,c(ztp1,length(crng),Se[s],Sp[s],assayID[s],crng,fill1))
            }
          }
        }
        tmp1 <- c(tmp1,tmp3)
      }
      pl.res[(cc[s]+1):cc[s+1]] <- tmp1
    }
    if(Rem == 1){
      yr1 <- rbinom(1,1,ifelse(Ytil1[N]==1,Se[S],1-Sp[S]))
      Zmat <- rbind(Zmat,c(yr1,1,Se[S],Sp[S],assayID[S],N,rep(-9,c.s[1]-1)))
    }
    if(Rem > 1){
      rid <- (M*c.s[1]+1):N
      ytr1 <- Ytil1[rid]
      zr2 <- rbinom(1,1,ifelse(sum(ytr1)>0,Se[S-1],1-Sp[S-1]))
      Zmat <- rbind(Zmat,c(zr2,Rem,Se[S-1],Sp[S-1],assayID[S-1],rid,rep(-9,c.s[1]-Rem)))
      if(zr2 > 0){
        yrm1 <- rbinom(Rem,1,ifelse(ytr1==1,Se[S],1-Sp[S]))
            Zmat <- rbind(Zmat,cbind(yrm1,1,Se[S],Sp[S],assayID[S],rid,matrix(-9,Rem,c.s[1]-1)))
      }
    }
  }
  ## Output
  return(Zmat)
}

## Simulating array testing data
array.gt.sim <- function(N,p=0.10,protocol=c("A2","A2M"),n,Se,Sp,assayID,Yt=NULL){
  ind.simulation <- function(Nr,prob=0.1,Se.ind,Sp.ind,yt=NULL){
    Yt.ind <- yt
    if( is.null(yt) ){
      Yt.ind <- stats::rbinom(Nr,1,prob)
    }
    for(k in 1:Nr){
      prb <- ifelse(Yt.ind>0, Se.ind, 1-Sp.ind)
      y.test <- stats::rbinom(Nr,1,prb)
    }
    return(y.test)
  }
  protocol <- match.arg(protocol)
  if(n <= 1) stop("Row size and column size must be larger than 1")
  if(n^2 > N) stop("The array size n*n is too large")
  L <- floor(N/n^2)
  N0 <- L*n^2
  if( !is.null(Yt) ){
    Ytil1 <- Yt 
  }else{
    Ytil1 <- stats::rbinom(N,1,p)
  }
  id1 <- cumsum( c(0,rep(n^2,L)) )
  Zmat <- Ymat <- NULL
  ## Simulating for two-stage array 
  if(protocol=="A2"){
    for(l in 1:L){
      Z_id <- matrix((id1[l]+1):id1[l+1],n,n)
      Ymat1 <- matrix(Ytil1[(id1[l]+1):id1[l+1]],n,n)
      R1 <- stats::rbinom(n,1,ifelse(rowSums(Ymat1)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,cbind(R1,n,Se[1],Sp[1],assayID[1],Z_id))
      C1 <- stats::rbinom(n,1,ifelse(colSums(Ymat1)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,cbind(C1,n,Se[1],Sp[1],assayID[1],t(Z_id)))
      for(i in 1:n){
        for(j in 1:n){
          T1 <- 0
          if(R1[i]==1 & C1[j]==1) T1 <- T1 + 1
          if(R1[i]==1 & sum(C1)==0) T1 <- T1 + 1
          if(sum(R1)==0 & C1[j]==1) T1 <- T1 + 1
          if(T1>=1){
            y1 <- stats::rbinom(1,1,ifelse(Ymat1[i,j]==1,Se[2],1-Sp[2]))
            Ymat <- rbind(Ymat,c(y1,1,Se[2],Sp[2],assayID[2],Z_id[i,j]))
          }
        }
      }
    }
    if(!is.null(Ymat)){
      Zmat <- rbind(Zmat,cbind(Ymat,matrix(-9,nrow(Ymat),n-1)))
    }
    idv.id <- paste( rep("Mem",n), 1:n, sep="" )
  }
  ## Simulating for three-stage array
  if(protocol=="A2M"){
    zfil <- matrix(-9,n,n^2-n)
    for(l in 1:L){
      aryid <- (id1[l]+1):id1[l+1]
      ytmp <- Ytil1[aryid]
      zary <- stats::rbinom(1,1,ifelse(sum(ytmp)>0,Se[1],1-Sp[1]))
      Zmat <- rbind(Zmat,c(zary,n*n,Se[1],Sp[1],assayID[1],aryid))
      if(zary==1){
        Z_id <- matrix((id1[l]+1):id1[l+1],n,n)
        Ymat1 <- matrix(Ytil1[(id1[l]+1):id1[l+1]],n,n)
        R1 <- stats::rbinom(n,1,ifelse(rowSums(Ymat1)>0,Se[2],1-Sp[2]))
        Zmat <- rbind(Zmat,cbind(R1,n,Se[2],Sp[2],assayID[2],Z_id,zfil))
        C1 <- stats::rbinom(n,1,ifelse(colSums(Ymat1)>0,Se[2],1-Sp[2]))
        Zmat <- rbind(Zmat,cbind(C1,n,Se[2],Sp[2],assayID[2],t(Z_id),zfil))
        for(i in 1:n){
          for(j in 1:n){
            T1 <- 0
            if(R1[i]==1 & C1[j]==1) T1 <- T1 + 1
            if(R1[i]==1 & sum(C1)==0) T1 <- T1 + 1
            if(sum(R1)==0 & C1[j]==1) T1 <- T1 + 1
            if(T1>=1){
              y1 <- stats::rbinom(1,1,ifelse(Ymat1[i,j]==1,Se[3],1-Sp[3]))
              Ymat <- rbind(Ymat,c(y1,1,Se[3],Sp[3],assayID[3],Z_id[i,j]))
            }
          }
        }
      }
    }
    if(!is.null(Ymat)){
      Zmat <- rbind(Zmat,cbind(Ymat,matrix(-9,nrow(Ymat),n*n-1)))
    }
    idv.id <- paste( rep("Mem",n), 1:n^2, sep="" )
  }
  ## Individual testing for the remainder sample
  Rem <- N-N0
  if( Rem > 0 ){
    S <- length(Se)
    ytest <- ind.simulation(Nr=Rem,Se.ind=Se[S],Sp.ind=Sp[S],yt=Ytil1[(N0+1):N])
        rmat <- matrix(-9,length(ytest),ncol(Zmat)-6)
    zfil <- cbind( ytest,1,Se[S],Sp[S],assayID[S],(N0+1):N,rmat )
    Zmat <- rbind( Zmat, zfil )
  }
  ## Output
  return(Zmat)
}

########################################
## Estimation efficiency
########################################

## Var of phat for individual testing
prop.var.ind <- function(p, N, Se, Sp){
  r <- Se + Sp - 1
  res <- ( (Se-r*(1-p))*(1-Se+r*(1-p)) )/(N*r^2)
  return( res )
}

## Var of phat for mater pooled testing
prop.var.mpt <- function(p, k, J, Se, Sp){
  r <- Se + Sp - 1
  num <- (Se - r*(1-p)^k)*(r*(1-p)^k + 1 - Se)
  den <- J*r^2*k^2*(1-p)^(2*(k-1))
  res <- num/den
  return( res )
}

## Var of phat for H2 using closed-form expressions
prop.var.H2 <- function(p, K, J, Se, Sp){
  r <- Se[1] + Sp[1] - 1
  p0 <- 1 - Se[1] + r*(1-p)^K
  d1p0 <- -K*r*(1-p)^(K-1)
  d2p0 <- K*(K-1)*r*(1-p)^(K-2)
  sm <- (J/p0)*( p0*d2p0 - (d1p0)^2 )
  for(m in 1:(K+1)){
    p1m <- p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    d1p1m <- d1p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    d2p1m <- d2p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    sm <- sm + (J/p1m)*choose(K,m-1)*( p1m*d2p1m - (d1p1m)^2 )
  }
  I.p <- -sm
  res <- 1/I.p
  return( res )
}  ## PERFECT!!

## Support function for 'prop.var.H2'
## This function computes the probability:
## P(Yt=1, Y1=1,...,Y_m-1=1, Ym=0,...,Yk=0)
p1.m <- function(p, m, K, Se, Sp){
  t1 <- (1-Sp)*Sp^(K-m+1)*(1-Sp)^(m-1)*(1-p)^K
  t2 <- rep(-9, K)
  for(d in 1:K){
    A1 <- d:0
    A2 <- 0:d
    sm <- 0
    for(s in 1:(d+1)){
      a1 <- A1[s]
      a2 <- A2[s]
      tmp1 <- choose(m-1, a1)*( Se*p )^a1*( (1-Sp)*(1-p) )^(m-1-a1)
      tmp2 <- choose(K-m+1, a2)*( (1-Se)*p )^a2*( Sp*(1-p) )^(K-m+1-a2)
      sm <- sm + tmp1*tmp2
    }
    t2[d] <- sm
  }
  res <- t1 + Se*sum(t2)
  return( res )
}  ## PERFECT!!

## Support function for 'prop.var.H2'
## First derivative of p1.m
d1p1.m <- function(p, m, K, Se, Sp){
  d1.t1 <- -K*(1-Sp)*Sp^(K-m+1)*(1-Sp)^(m-1)*(1-p)^(K-1)
  d1.t2 <- rep(-9, K)
  for(d in 1:K){
    A1 <- d:0
    A2 <- 0:d
    sm <- 0
    for(s in 1:(d+1)){
      a1 <- A1[s]
      a2 <- A2[s]
      c1m <- choose(m-1,a1)*choose(K-m+1,a2)*Se^a1*(1-Se)^a2*(1-Sp)^(m-1-a1)*Sp^(K-m+1-a2)
      sm <- sm + c1m*( (a1+a2)*p^(a1+a2-1)*(1-p)^(K-a1-a2) 
	                 - (K-a1-a2)*p^(a1+a2)*(1-p)^(K-a1-a2-1) )
    }
    d1.t2[d] <- sm
  }
  res <- d1.t1 + Se*sum(d1.t2)
  return( res )
}  ## PERFECT!!

## Support function for 'prop.var.H2'
## Second derivative of p1.m
d2p1.m <- function(p, m, K, Se, Sp){
  d2.t1 <- K*(K-1)*(1-Sp)*Sp^(K-m+1)*(1-Sp)^(m-1)*(1-p)^(K-2)
  d2.t2 <- rep(-9, K)
  for(d in 1:K){
    A1 <- d:0
    A2 <- 0:d
    sm <- 0
    for(s in 1:(d+1)){
      a1 <- A1[s]
      a2 <- A2[s]
      c1m <- choose(m-1,a1)*choose(K-m+1,a2)*Se^a1*(1-Se)^a2*(1-Sp)^(m-1-a1)*Sp^(K-m+1-a2)
      t1m <- (a1+a2)*(a1+a2-1)*p^(a1+a2-2)*(1-p)^(K-a1-a2)
      t2m <- (a1+a2)*(K-a1-a2)*p^(a1+a2-1)*(1-p)^(K-a1-a2-1)
      t3m <- (K-a1-a2)*(K-a1-a2-1)*p^(a1+a2)*(1-p)^(K-a1-a2-2)
      sm <- sm + c1m*( t1m - 2*t2m + t3m )
    }
    d2.t2[d] <- sm
  }
  res <- d2.t1 + Se*sum(d2.t2)
  return( res )
}

## Observed Fisher information using Louis's method.
prop.info.louis <- function(gtData, p, ngit){
  Memb <- gtData[ ,-(1:5)]
  N <- max(Memb)
  maxAssign <- max(as.numeric(table(Memb[Memb > 0])))
  ytm <- matrix(-9,N,maxAssign)
  tmp <- as.matrix( Memb )
  vec <- 1:nrow(gtData)
  for(d in 1:N){
    tid <- tmp==d
    store <- NULL
    for(i in 1:ncol(tmp)){
      store <- c(store,vec[tid[ ,i]])
    }
    ytm[d,1:length(store)] <- sort(store)
  }

  ## Generating individual true disease 
  ## statuses at the given true value p
  Yt <- stats::rbinom(N,1,p)
  Ytmat <- cbind(Yt,rowSums(ytm>0),ytm)

  ## Specifying the number of Gibbs iterates and
  ## other variables for the Fortran subroutine  
  nburn <- 0
  GI <- ngit + nburn
  Ycol <- ncol(Ytmat)
  SeSp <- gtData[ ,3:4]
  Z <- gtData[ ,-(3:5)]
  Zrow <- nrow(Z)
  Zcol <- ncol(Z)

  ## Variance in Fortran using Louis's (1982) method
  U <- matrix(stats::runif(N*GI),nrow=N,ncol=GI)
  Info <- .Call("cvondknachom_c",as.double(p),as.integer(Ytmat),
                as.integer(Z),as.integer(N),as.double(SeSp),as.integer(Ycol),
                as.integer(Zrow),as.integer(Zcol),as.double(U),as.integer(GI),
                as.integer(nburn), PACKAGE="groupTesting")
  # Output
  return( Info )
}

## Var of phat for general hierarchical testing.
## Louis's method has been used to estimate 
## the observed Fisher information.
prop.var.hier <- function(N, p, Se, Sp, assayID, kmat, nstg, ngit, nrep, seed){
  set.seed(seed)
  res <- rep(-9, nrow(kmat))
  ind_iter <- matrix(-9, nrow(kmat), nrep)
  for(i in 1:nrow(kmat)){
    h.data <- lapply(X=rep(N,nrep), FUN=hier.gt.sim, p=p, S=nstg,  
                     psz=kmat[i, ], Se=Se, Sp=Sp, assayID=assayID)
    h.info <- sapply(X=h.data, FUN=prop.info.louis, p=p, ngit=ngit)
    res[i] <- 1/mean(h.info)
    ind_iter[i, ] <- (1:nrep)/cumsum(h.info)
  }
  return( list("sample_mean"=res, "iterations"=ind_iter) )
}

## Var of phat for general array testing.
## Louis's method has been used to estimate 
## the observed Fisher information.
prop.var.array <- function(N, p, protocol, nvec, Se, Sp, assayID, ngit, nrep, seed){
  set.seed(seed)
  res <- rep(-9, length(nvec))
  ind_iter <- matrix(-9, length(nvec), nrep)
  for(i in 1:length(nvec)){
    a.data <- lapply(X=rep(N,nrep), FUN=array.gt.sim, p=p, protocol=protocol, 
                     n=nvec[i], Se=Se, Sp=Sp, assayID=assayID)
    a.info <- sapply(X=a.data, FUN=prop.info.louis, p=p, ngit=ngit)
    res[i] <- 1/mean(a.info)
    ind_iter[i, ] <- (1:nrep)/cumsum(a.info)
  }
  return( list("sample_mean"=res, "iterations"=ind_iter) )
}

## Relative estimation efficiency for MPT
REE.mpt <- function(p, Se, Sp, kvec){
  r <- Se + Sp - 1
  res <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    k <- kvec[i]
    num <- (Se - r*(1-p)^k)*(1 - Se + r*(1-p)^k)
    dt1 <- k*(1-p)^( 2*(k-1) )
    dt2 <- (1 - Sp + r*p)*(Sp - r*p)
    den <- dt1*dt2
    res[i] <- num/den
  }
  return( res )
}

## Relative estimation efficiency for 
## two-stage hierarchical testing
REE.H2 <- function(p, Se, Sp, kvec){
  r <- Se + Sp - 1
  den <- (Se - r*(1-p))*(1-Se+r*(1-p))
  res <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    k <- kvec[i]
    Ip.ind <- r^2*k/den
    Ip.h2 <- Ip.H2(p=p, K=k, Se=Se, Sp=Sp)
    res[i] <- Ip.ind/Ip.h2
  }
  return( res )
}

## Support function for 'REE.H2'
## Fisher information for H2
Ip.H2 <- function(p, K, Se, Sp){
  r <- Se + Sp - 1
  p0 <- 1 - Se + r*(1-p)^K
  d1p0 <- -K*r*(1-p)^(K-1)
  d2p0 <- K*(K-1)*r*(1-p)^(K-2)
  sm <- (1/p0)*( p0*d2p0 - (d1p0)^2 )
  for(m in 1:(K+1)){
    p1m <- p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    d1p1m <- d1p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    d2p1m <- d2p1.m(p=p, m=m, K=K, Se=Se, Sp=Sp)
    sm <- sm + (1/p1m)*choose(K,m-1)*( p1m*d2p1m - (d1p1m)^2 )
  }
  Ip <- -sm
  return( Ip )
}  ## PERFECT!!

########################################
## Cost efficiency
########################################

## Relative cost efficiency for MPT
RCE.mpt <- function(p, Se, Sp, kvec){
  r <- Se + Sp - 1
  res <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    k <- kvec[i]
    num <- (Se - r*(1-p)^k)*(1 - Se + r*(1-p)^k)
    dt1 <- k*(1-p)^( 2*(k-1) )
    dt2 <- (1 - Sp + r*p)*(Sp - r*p)
    den <- dt1*dt2
    res[i] <- num/den
  }
  return( res/kvec )
}

## Relative cost efficiency for all 
## hierarchical & array testing
propMLE.gt <- function(gtData,p0,ngit=2000,maxit=200,tol=1e-03){
  ## This block tracks the individuals assigned to a pool.
  ## Note: 'gtData' must have the required specific structure.
  Memb <- gtData[ ,-(1:5)]
  N <- max(Memb)
  maxAssign <- max(as.numeric(table(Memb[Memb > 0])))
  ytm <- matrix(-9,N,maxAssign)
  tmp <- as.matrix( Memb )
  vec <- 1:nrow(gtData)
  for(d in 1:N){
    tid <- tmp==d
    store <- NULL
    for(i in 1:ncol(tmp)){
      store <- c(store,vec[tid[ ,i]])
    }
    ytm[d,1:length(store)] <- sort(store)
  }

  ## Generating individual true disease statuses
  ## at the initial parameter value p0
  Yt <- stats::rbinom(N,1,p0)
  Ytmat <- cbind(Yt,rowSums(ytm>0),ytm)

  ## Some global variables
  Ycol <- ncol(Ytmat)
  SeSp <- gtData[ ,3:4]
  Z <- gtData[ ,-(3:5)]
  Zrow <- nrow(Z)
  Zcol <- ncol(Z)

  ## The total number of Gibbs iterates
  nburn <- 0
  GI <- ngit + nburn
  
  ## Initial value of the parameter
  p1 <- p0
  p0 <- p0 + 2*tol
  s <- 1
  convergence <- 0

  ## The EM algorithm starts here
  while(abs(p1-p0) > tol){ 
    p0 <- p1
    U <- matrix(stats::runif(N*GI),nrow=N,ncol=GI)
    
    ## Gibbs sampling in Fortran to approximate the E-step
    res <- .Call("gbsonedhom_c",as.double(p0),as.integer(Ytmat),
                 as.integer(Z),as.integer(N),as.double(SeSp),as.integer(Ycol),
                 as.integer(Zrow),as.integer(Zcol),as.double(U),as.integer(GI),
                 as.integer(nburn), PACKAGE="groupTesting")
    temp <- sum( res )/ngit
    
    ## M-step: The parameter p is updated here
    p1 <- temp/N
    
    ## Terminate the EM algorithm if it exceeds max iteration
    if(s >= maxit){
      convergence <- 1
      break
    }
    s <- s + 1
  }
  return( p1 )
}

prop.CPUI.hier <- function(N, p, Se, Sp, assayID, kmat, nstg, ngit, nrep, maxit, tol, seed){
  set.seed(seed)
  res <- rep(-9, nrow(kmat))
  ind_iter <- matrix(-9, nrow(kmat), nrep)
  for(i in 1:nrow(kmat)){
    h.data <- lapply(X=rep(N,nrep), FUN=hier.gt.sim, p=p, S=nstg,
                     psz=kmat[i, ], Se=Se, Sp=Sp, assayID=assayID)
    T <- sapply(X=h.data, FUN=nrow)
    phat <- sapply(X=h.data, FUN=propMLE.gt, p0=p, ngit=ngit, maxit=maxit, tol=tol)
    res[i] <- mean( T*(phat-p)^2 )
    ind_iter[i, ] <- cumsum( T*(phat-p)^2 )/(1:nrep)
  }
  return( list("sample_mean"=res, "iterations"=ind_iter) )
}

prop.CPUI.array <- function(N, p, protocol, nvec, Se, Sp, assayID, ngit, nrep, maxit, tol, seed){
  set.seed(seed)
  res <- rep(-9, length(nvec))
  ind_iter <- matrix(-9, length(nvec), nrep)
  for(i in 1:length(nvec)){
    a.data <- lapply(X=rep(N,nrep), FUN=array.gt.sim, p=p, protocol=protocol, 
                     n=nvec[i], Se=Se, Sp=Sp, assayID=assayID)
    T <- sapply(X=a.data, FUN=nrow)
    phat <- sapply(X=a.data, FUN=propMLE.gt, p0=p, ngit=ngit, maxit=maxit, tol=tol)
    res[i] <- mean( T*(phat-p)^2 )
    ind_iter[i, ] <- cumsum( T*(phat-p)^2 )/(1:nrep)
  }
  return( list("sample_mean"=res, "iterations"=ind_iter) )
}

########################################
## Test efficiency
########################################

## Relative testing efficiency (RTE)
RTE.H2 <- function(kvec, p, Se, Sp){
  output <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    k <- kvec[i]
    config <- rbind( 1, 1:k )
    res <- binGroup2::opChar1(algorithm="D2", p=p, Se=Se, Sp=Sp, hier.config=config, print.time=FALSE)
    output[i] <- as.numeric(binGroup2::ExpTests(res)[2])
  }
  cbind(kvec, 1, output)
}

RTE.H3 <- function(kvec, p, Se, Sp){
  kmat <- S3.configs(psz=kvec)
  output <- rep(-9, nrow(kmat))
  for(i in 1:nrow(kmat)){
    gs <- kmat[i, ]
    config <- rbind(rep(1,gs[1]), rep(1:(gs[1]/gs[2]),each=gs[2]), 1:gs[1])
    res <- binGroup2::opChar1(algorithm="D3", p=p, Se=Se, Sp=Sp, hier.config=config, print.time=FALSE)
    output[i] <- as.numeric(binGroup2::ExpTests(res)[2])
  }
  cbind(kmat, output)
}

RTE.H4 <- function(kvec, p, Se, Sp){
  kmat <- S4.configs(psz=kvec)
  output <- rep(-9, nrow(kmat))
  for(i in 1:nrow(kmat)){
    gs <- kmat[i, ]
    v1 <- rep(1, gs[1])
    v2 <- rep(1:(gs[1]/gs[2]),each=gs[2])
    v3 <- rep(1:(gs[1]/gs[3]),each=gs[3])
    config <- rbind(v1, v2, v3, 1:gs[1])
    res <- binGroup2::opChar1(algorithm="D4", p=p, Se=Se, Sp=Sp, hier.config=config, print.time=FALSE)
    output[i] <- as.numeric(binGroup2::ExpTests(res)[2])
  }
  cbind(kmat, output)
}

RTE.A2 <- function(kvec, p, Se, Sp){
  output <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    n <- kvec[i]
    res <- binGroup2::opChar1(algorithm="A2", p=p, Se=Se, Sp=Sp, rowcol.sz=n, print.time=FALSE)
    output[i] <- as.numeric(binGroup2::ExpTests(res)[2])
  }
  cbind(kvec, 1, output)
}

RTE.A2M <- function(kvec, p, Se, Sp){
  output <- rep(-9, length(kvec))
  for(i in 1:length(kvec)){
    n <- kvec[i]
    res <- binGroup2::opChar1(algorithm="A2M", p=p, Se=Se, Sp=Sp, rowcol.sz=n, print.time=FALSE)
    output[i] <- as.numeric(binGroup2::ExpTests(res)[2])
  }
  cbind(kvec^2, kvec, 1, output)
}

## Main function for relative testing efficiency (RTE)
prop.RTE <- function(p, Se, Sp, initial.psz, protocol){
  M.psz <- sort(unique(initial.psz))
  if(protocol=="MPT"){
    S <- 1
    res <- cbind(M.psz, round(1/M.psz, 4))
  }
  if(protocol=="H2"){
    S <- 2
    res <- RTE.H2(kvec=M.psz, p=p, Se=Se, Sp=Sp)
  }
  if(protocol=="H3"){
    S <- 3
    res <- RTE.H3(kvec=M.psz, p=p, Se=Se, Sp=Sp)
  }
  if(protocol=="H4"){
    S <- 4
    res <- RTE.H4(kvec=M.psz, p=p, Se=Se, Sp=Sp)
  }
  if(protocol=="A2"){
    S <- 2
    res <- RTE.A2(kvec=M.psz, p=p, Se=Se, Sp=Sp)
  }
  if(protocol=="A2M"){
    S <- 3
    res <- RTE.A2M(kvec=M.psz, p=p, Se=Se, Sp=Sp)
  }
##  rownames(res) <- ""
  colnames(res) <- c( paste("PSZ.S", 1:S, sep=""), "RTE" )
  return( res )
}

