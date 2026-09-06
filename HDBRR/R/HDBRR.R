require(numDeriv)
require(parallel)
require(bigstatsr)
require(bigparallelr)
require(MASS)

.sysinfo <- Sys.info()
.OS <- .sysinfo[['sysname']]
.mc1.cores = getOption("mc.cores", 2L)
if(.OS == "Windows"){
  .mc1.cores = 1L
}

bigparallelr::set_blas_ncores(nb_cores())


.mcsapply<-function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, mc.preschedule = TRUE,
                     mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = .mc1.cores,
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL )
{
  answer <- mclapply(X = X, FUN = FUN, ...,mc.preschedule = mc.preschedule,
                     mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores,
                     mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive, affinity.list = affinity.list)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

.big_fat_svd <- function(x,xxt=NULL){

  # Compute t(x) * x
  if(is.null(xxt)){
    x <- big_transpose(x)
    xxt <- big_crossprodSelf(x, big_scale(center = FALSE, scale = FALSE))
  }
  # Get v and d where a = u * d * t(v) the SVD of a
  v <- FBM(ncol(x), ncol(x))
  eig <- eigen(xxt[]+diag(1E-6,ncol(x)))
  v[] <- eig$vectors
  d <- sqrt(eig$values)

  # Get u, it will be of the same size of x
  # so that I store it as a FBM.
  u <- FBM(nrow(x), ncol(x))
  big_apply(u, a.FUN = function(X, ind, x, v, d) {
    X[ind, ] <- sweep(x[ind, ] %*% v[], 2, d, "/")
  }, a.combine = 'c', ind = rows_along(u),
  x = x, v = v, d = d)
  return(list(d=d,u=v,v=u,xxt=xxt))
}


.big_fat_tqr <- function(x,qmat=TRUE,xxt=NULL){
  x <- big_transpose(x)

  # Compute t(X) %*% X
  if(is.null(xxt)){
    xxt <- big_crossprodSelf(x, big_scale(center = FALSE, scale = FALSE))
  }
  xxt[]<- xxt[]+diag(1E-6,ncol(x))
  # Get q and r where a = q*r the qr of a
  r <- FBM(ncol(x), ncol(x))
  rinv <- FBM(ncol(x), ncol(x))
  r[] <- chol(xxt[])
  if(qmat){
    rinv[] <- backsolve(r[],diag(ncol(x)))
    # Get q, it will be of the same size of x
    # so that I store it as a FBM.
    q <- FBM(nrow(x), ncol(x))
    big_apply(q, a.FUN = function(X, ind, x, rinv) {
      X[ind, ] <- sweep(x[ind, ] %*% rinv[],2,1,"*")
    }, a.combine = 'c', ind = rows_along(q),
    x = x, rinv = rinv)
  }
  return(list(q=q,r=r,xxt=xxt))
}


matop <- function(y=NULL,X,method = c("svd","qr"), bigmat = TRUE){
  n <- nrow(X)
  p <- ncol(X)
  Xfbm <- FBM(n, p)
  Xfbm[] <- X
  if(p > n && bigmat){
    if(method[1]!="svd")
    {
      qr.X <- .big_fat_tqr(Xfbm,qmat=TRUE)
      q <- qr.X$q[]
      r <- qr.X$r[]
    }else{
      svd.X <- .big_fat_svd(Xfbm)
      ev <- svd.X$d^2
      D <-  svd.X$d
      L <- svd.X$u
      R <- svd.X$v
    }
  }

  if(p <= n || !bigmat){
    if(method[1] != "svd")
    {
      qr.X <- qr(X)
      r <- qr.R(qr.X)
      q <- qr.Q(qr.X)
    }else{
      svd.X <- svd(X)
      ev <- svd.X$d^2
      D <-  svd.X$d
      L <- svd.X$u
      R <- svd.X$v
      Lfbm <- FBM(nrow(L),ncol(L))
      Rfbm <- FBM(nrow(R),ncol(R))
      Lfbm[] <- L
      Rfbm[]  <- R
      L <- Lfbm
      R <- Rfbm
    }
  }

  if(method[1]=="svd"){
  	if(!is.null(y)){
  		Ly <- big_cprodMat(L,as.matrix(y,ncol=1))
  		} else Ly <- NULL
    LD <- big_prodMat(L,diag(D))
    return(list(y=y,X=X,D=D,L=L,R=R,ev=ev,LD=LD,Ly=Ly,n=n,p=p))
  } else{
    return(list(y=y,X=X,R=r,n=n,p=p,Q=q))
  }
}
############################################################


.logdn0 <- function(u,a1,a2,b1,b2,svdx,matdec = c("svd","qr")){
  y <- svdx$y
  n <- svdx$n
  p <- svdx$p
  X <- svdx$X

  if(matdec[1] == "svd"){
    D <-  svdx$D
    ev <- svdx$ev
    L <- svdx$L
    R <- svdx$R
    Ly <- svdx$Ly

    cte <- -n/2*log(2*pi)+(a1/2)*log(b1/2)+(a2/2)*log(b2/2)-lgamma(a1/2)-lgamma(a2/2)+lgamma((n+a1+a2)/2)+(n+a1+a2)/2*log(2)
    ldn <- cte-1/2*sum(log((1-u)*ev+u))+(n-p)/2*log(u)*(p > n)
    ldn <- ldn+((a1+n)/2-1)*log(1-u)+((a2+p)/2-1)*log(u)
    if(n > p){
      W <- diag(D/(ev+u/(1-u)))
      WLy <- crossprod(W,Ly)
      betahat <- crossprod(t(R[]),WLy)
      yhat <- crossprod(t(L[]),D*WLy)
      SSE <- sum((y-yhat)^2)
      fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
    } else{
      fu <- u*(1-u)*sum(Ly^2/(ev*(1-u)+u))+(1-u)*b1+u*b2
    }
    ldn <- ldn-(n+a1+a2)/2*log(fu)
  }else{
    cte <- -n/2*log(2*pi)+(a1/2)*log(b1/2)+(a2/2)*log(b2/2)-lgamma(a1/2)-lgamma(a2/2)+lgamma((n+a1+a2)/2)+(n+a1+a2)/2*log(2)
      Q <- svdx$Q
      R <- svdx$R
      Rfbm <- FBM(n, n)
      Qfbm <- FBM(p, n)
      Kfbm <- FBM(n, n)
      Xfbm <- FBM(n, p)
      Rfbm[] <- R
      Qfbm[] <- Q
      Xfbm[] <- X
      K <- big_tcrossprodSelf(Rfbm, big_scale(center = FALSE, scale = FALSE))
      Kfbm[] <- K[]+u/(1-u)*diag(n)
      svd.K <- .big_fat_svd(Kfbm)
      Afbm <- FBM(n,n)
      Afbm[] <- big_prodMat(svd.K$u,diag(1/sqrt(svd.K$d)))
      invK <- big_tcrossprodSelf(Afbm,big_scale(center = FALSE, scale = FALSE))
      ldn <- cte+1/2*log(det(invK[]))
      ldn <- ldn+((a1+n)/2-1)*log(1-u)+((a2+p)/2-1)*log(u)-(p-n)/2*log((1-u)/u)*(p>n)
      betahat <- big_prodMat(Qfbm,big_prodMat(invK,big_prodMat(Rfbm,as.matrix(y,ncol=1))))
      yhat <- big_prodMat(Xfbm,betahat)
      SSE <- sum((y-yhat)^2)
      fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
      ldn <- ldn-(n+a1+a2)/2*log(fu)
  }
  return(ldn)
}



.logdn <- function(u,a1,a2,b1,b2,svdx,vpapp=TRUE,method = c("svd","qr")){
  y <- svdx$y
  X <- svdx$X
  n <- svdx$n
  p <- svdx$p

  if(method[1] == "svd"){
    ev <- svdx$ev
    D <-  svdx$D
    L <- svdx$L
    R <- svdx$R
    LD <- svdx$LD
    Ly <- svdx$Ly
    W <- diag(D/(ev+u/(1-u)))
    WLy <- tcrossprod(W,t(Ly))#big_cprodMat(W,Ly)
    yhat <- tcrossprod(L,t(D*WLy))#big_prodMat(L,D*WLy[])
    R <- R[,1:min(n,p)]
    betahat <- tcrossprod(R,t(WLy))#big_prodMat(Rfbm,WLy[])
    ev1 <- D^2+u/(1-u)
    if(p>n){
      ev2 <- c(ev1,rep(u/(1-u),p-n))
      fu <- u*(1-u)*sum(Ly^2/(ev*(1-u)+u))+(1-u)*b1+u*b2
    } else {
      ev2 <- ev1
      SSE <- sum((y-yhat)^2)
      fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
    }
    invxx <- sapply(1:p,function(i)sum(R[i,]^2/ev2[1:min(n,p)]))
    vhat <- (n+a1+a2-1)/fu
    varb <- fu/((n+a1+a2-2)*(1-u))*invxx
    sigsqhat <- 1/(vhat*(1-u))
    sigbsqhat <- 1/(vhat*u)
    if(!vpapp){
      varyhat <- diag(tcrossprod(tcrossprod(LD,diag(1/ev1)),LD))
    } else varyhat <- rep(0,n)
  }
  else{
    Q <- svdx$Q
    R <- svdx$R

    # if(n >= p){
    # sig <- solve(crossprod(R,R)+diag(u/(1-u),p))
    # betahat <- t(tcrossprod(tcrossprod(sig,X),t(y)))
    # yhat <- t(tcrossprod(X,betahat))
    # SSE <- sum((y-t(yhat))^2)
    # fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
    # invxx <- diag(sig)
    # if(!vpapp){
    # varyhat0 <- tcrossprod(crossprod(t(R),sig),R)
    # varyhat <- diag(tcrossprod(crossprod(t(Q),varyhat0),Q))
    # } else varyhat <- rep(0,n)
    # }
    # else{
    # sig <- matrix(0,p,p)
    # bloq1 <- solve(tcrossprod(R,R)+diag(u/(1-u),n))
    # sig[1:n,1:n] <- bloq1
    # sig[(n+1):p,(n+1):p] <- diag((1-u)/u,p-n)
    # betahat <- tcrossprod(tcrossprod(crossprod(t(Q),bloq1),t(R)),t(y))
    # betahat <- t(betahat)
    # yhat <- tcrossprod(tcrossprod(crossprod(R,bloq1),t(R)),t(y))
    # yhat <- t(yhat)

    ###########################################
    if(n > p){
      y <- svdx$y
      Q <- svdx$Q
      R <- svdx$R
      n <- svdx$n
      p <- svdx$p
      X <- svdx$X
      sig <- solve(crossprod(R,R)+diag(u/(1-u),p))
      betahat <- t(tcrossprod(tcrossprod(sig,X),t(y)))
      yhat <- t(tcrossprod(X,betahat))
      SSE <- sum((y-t(yhat))^2)
      fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
      invxx <- diag(sig)
      if(!vpapp){
        varyhat0 <- tcrossprod(crossprod(t(R),sig),R)
        varyhat <- diag(tcrossprod(crossprod(t(Q),varyhat0),Q))
      } else varyhat <- rep(0,n)
      vhat <- (n+a1+a2-1)/fu
      varb <- fu/((n+a1+a2-2)*(1-u))*invxx
      sigsqhat <- 1/(vhat*(1-u))
      sigbsqhat <- 1/(vhat*u)
    }
    else{
      Rfbm <- FBM(n, n)
      Qfbm <- FBM(p, n)
      Kfbm <- FBM(n, n)
      Xfbm <- FBM(n, p)
      Rfbm[] <- R
      Qfbm[] <- Q
      Xfbm[] <- X
      K <- big_tcrossprodSelf(Rfbm, big_scale(center = FALSE, scale = FALSE))
      Kfbm[] <- K[]+u/(1-u)*diag(n)
      svd.K <- .big_fat_svd(Kfbm)
      Afbm <- FBM(n,n)
      Afbm[] <- big_prodMat(svd.K$u,diag(1/sqrt(svd.K$d)))
      invK <- big_tcrossprodSelf(Afbm,big_scale(center = FALSE, scale = FALSE))
      betahat <- big_prodMat(Qfbm,big_prodMat(invK,big_prodMat(Rfbm,as.matrix(y,ncol=1))))
      yhat <- big_prodMat(Xfbm,betahat)
      #############################################
      SSE <- sum((y-t(yhat))^2)
      fu <- (1-u)*(SSE+b1)+u*(as.numeric(sum(betahat^2))+b2)
      invxx <- diag(tcrossprod(crossprod(t(Q),invK[]),Q))
      vhat <- (n+a1+a2-1)/fu
      varb <- fu/((n+a1+a2-2)*(1-u))*invxx
      sigsqhat <- 1/(vhat*(1-u))
      sigbsqhat <- 1/(vhat*u)
      if(!vpapp){
        varyhat <- diag(tcrossprod(crossprod(R,invK[]),t(R)))
      } else varyhat <- rep(0,n)
    }

  }

  return(list(betahat=betahat,yhat=yhat,varyhat=varyhat,varb=varb,
              sigsqhat=sigsqhat,sigbsqhat=sigbsqhat,vhat=vhat,invxx=invxx))
}



.postu <- function(u,a1=1E-4,a2=1E-4,b1=1E-4,b2=1E-4, svdx, ncores, method = c("svd","qr")){
  s <- length(u)
  n <- svdx$n
  p <- svdx$p
  X <- svdx$X

  lpostu <- .mcsapply(1:s,function(i){
    .logdn(u[i],a1,a2,b1,b2,svdx,method=method)},mc.cores=ncores)

  betahat <- matrix(unlist(lpostu["betahat",]),ncol=p,byrow=T)
  yhat <- matrix(unlist(lpostu["yhat",]),ncol=n,byrow=T)
  varyhat <- matrix(unlist(lpostu["varyhat",]),ncol=n,byrow=T) # nuevo
  varb <- matrix(unlist(lpostu["varb",]),ncol=p,byrow=T)
  sigsqhat <- matrix(unlist(lpostu["sigsqhat",]),ncol=1)
  sigbsqhat <- matrix(unlist(lpostu["sigbsqhat",]),ncol=1)
  vhat <- matrix(unlist(lpostu["vhat",]),ncol=1)
  invxx <- matrix(unlist(lpostu["invxx",]),ncol=p,byrow=T)

  return(list(betahat=betahat,yhat=yhat,varyhat=varyhat,varb=varb,sigsqhat=sigsqhat,
              sigbsqhat=sigbsqhat,vhat=vhat,invxx=invxx))
}


HDBRR <- function(y, X, n0 = 5, p0 = 5, s20 = NULL, d20 = NULL, h=0.5, intercept = TRUE,
      vpapp = TRUE,npts = NULL,c = NULL, corpred = NULL, method = c("svd","qr"),bigmat = TRUE, ncores = 2, svdx=NULL){

  sysinfo <- Sys.info()
  OS <- sysinfo[['sysname']]
  if(OS == 'Windows'){
    ncores = 1
  }
  cl <- match.call()
  m <- match.call(expand.dots = FALSE)

  n <- nrow(X)
  p <- ncol(X)

  names <- colnames(X)

  if(is.null(names)){
    colnames(X) <- paste("X",1:p,sep="")
  }

  ###################### cambios ####################
  if(!is.vector(y)) y <- as.vector(y)
  ysc <- scale(y,scale=FALSE)
  ybar <- mean(y,na.rm=TRUE)
  if(intercept){
    X <- cbind(rep(1,n),X)
  } else{
    y <- ysc
  }

  if(is.null(s20)){
    s20 <- (quantile(ysc,0.8413447, na.rm=TRUE)-quantile(ysc,0.1586553, na.rm=TRUE))/2
    s20 <- s20^2
  }

  if(is.null(d20)){
    s2x <- sum(apply(X^2,2,mean))
    d20  <-  s20*h*(p0-1)/((1-h)*p0*s2x)
  }

  a1 <- n0
  b1 <- n0*s20
  a2 <-  p0
  b2 <- p0*d20
  ####################################################


  if(is.null(npts)) npts <- 200
  #  if(intercept) X <- cbind(rep(1,n),X)
  # if(!is.vector(y)) y <- as.vector(y)
  Xall <- X
  yall <- y
  isNA <- is.na(y)
  whichNa <- which(isNA)
  y <- y[!isNA]
  X <- X[!isNA,]
  if(is.null(svdx)){
  	svdx <- matop(y,X,bigmat = bigmat,method = "svd")
  	}
  u0 <- seq(1E-6,1-1E-6,length=10)
  dn0 <- sapply(1:10,function(i).logdn0(u0[i],a1,a2,b1,b2,svdx,matdec="svd"))
  u0 <- u0[which.max(dn0)]
  par0 <- suppressWarnings(optim(u0,.logdn0,method="L-BFGS-B",lower=1E-6,upper=1-1E-10,
                      a1=a1,a2=a2,b1=b1,b2=b2,svdx=svdx,matdec= "svd",control=list(fnscale=-1),hessian=F))
  umode <- par0$par
  hess <- suppressWarnings(hessian(.logdn0,umode,a1=a1,a2=a2,b1=b1,b2=b2,svdx=svdx,matdec="svd"))

  if(!is.nan(hess)){
    sd0 <- 1/sqrt(-hess)
    a <- umode*(umode*(1-umode)/sd0^2-1)
    b <- umode*(1-umode)/sd0^2-a-1
    u <- seq(qbeta(0.001,a,b),qbeta(0.999,a,b),length=npts)
  }else{
    u0 <- seq(max(0.001,umode-1/2),min(umode+1/2,0.999),length = npts)
    dn <- sapply(1:npts,function(i).logdn0(u0[i],a1,a2,b1,b2,svdx,matdec="svd"))
    idint <- range(which(exp(dn-max(dn))>0.001))
    u <- seq(u0[idint[1]],u0[idint[2]],length = npts)
  }

  dn <- sapply(1:npts,function(i).logdn0(u[i],a1,a2,b1,b2,svdx,matdec="svd"))
  dn <- exp(dn-max(dn))
  step <- 1/npts
  probs  <- step*dn
  postu <- probs <- probs/sum(probs)

  if(method[1]=="qr"){
  if(is.null(svdx)){
  	svdx <- matop(y,X,bigmat = bigmat,method = "qr")
  	}  
    margu <- .postu(umode,a1,a2,b1,b2,svdx,ncores = ncores,method = method)
    probs <- 1
  } else{
    margu <- .postu(u,a1,a2,b1,b2,svdx,ncores = ncores,method = method)
  }

  #sigsqb <- margu$varb
  betahat <- c(probs%*%margu$betahat)
  varb <-probs%*%margu$varb
  varb <- c(t(varb)+(t(margu$betahat)-c(betahat))^2%*%probs)

  ########### cambios ##################################
  if(sum(isNA)>0){
    yhat <-  c(Xall%*%betahat)} else {
      yhat <-  c(probs%*%margu$yhat)
    }
  ######################################################

  sigsqhat <- c(probs%*%margu$sigsqhat)
  sigbsqhat <- c(probs%*%margu$sigbsqhat)
  if(method[1] == "svd"){
    if(!vpapp){
      varyhat <- c(t(probs%*%margu$varyhat)+(t(margu$yhat)-c(yhat))^2%*%probs)
    } else {
      ev <- svdx$D^2+umode/(1-umode)
      varyhat <- diag(tcrossprod(tcrossprod(svdx$LD,diag(1/ev)),svdx$LD))
    }
    varpred <- varyhat+sigsqhat
    sdyhat <- sqrt(varyhat)
    sdpred <- sqrt(varpred)
    uhat <- c(u%*%probs)
  } else{
    if(!vpapp){
      varyhat <- c(t(probs%*%margu$varyhat)+(t(margu$yhat)-c(yhat))^2%*%probs)
    } else{
      if(n >= p){
        ev <- solve(crossprod(svdx$R,svdx$R)+umode/(1-umode))
        varyhat <- diag(tcrossprod(crossprod(t(svdx$Q),tcrossprod(crossprod(t(svdx$R),ev),svdx$R)),svdx$Q))
      } else{
        ev <- solve(tcrossprod(svdx$R,svdx$R)+umode/(1-umode))
        varyhat <- diag(tcrossprod(crossprod(svdx$R,ev),t(svdx$R)))
      }
    }
    varpred <- varyhat+sigsqhat
    sdyhat <- sqrt(varyhat)
    sdpred <- sqrt(varpred)
    uhat <- umode
  }


  phat <-  delta  <- NULL
  if(!is.null(c)){
    vhat <- 1/sigsqhat+1/sigbsqhat
    lambda <- c(uhat/(1-uhat))
    SSE <- sum((y-yhat[!isNA])^2)
    ssx <- sapply(1:(p+intercept),function(i)sum(X[,i]^2))
    beta.cond <- sapply(1:(p+intercept),function(i)t(X[,i])%*%(y-yhat[!isNA]+X[,i]*betahat[i])/(ssx[i]+lambda))
    #phi <- 1/2
    phat <- 1-exp(-uhat*vhat*betahat^2/2+vhat*(1-uhat)*(ssx+uhat/(1-uhat))*(betahat-beta.cond)^2/2)
    #phat <- 1-(1-phi)*sqrt(lambda/(ssx+lambda))*exp(-vhat*(1-uhat)*ssx*betahat^2/2)*exp(-uhat*vhat*betahat^2/2)/exp(-vhat*(1-uhat)*(ssx+uhat/(1-uhat))*(betahat-beta.cond)^2/2)
    delta <- sqrt(sigbsqhat*2*log(c)*c^2/(c^2-1))
  }

  corr <- edf <- NULL
  if(method[1] == "svd" && !is.null(corpred)){
    if(corpred != "eb"){
      num1 <- sapply(1:length(u),function(i)(1-u[i])*svdx$ev)
      pj <- num1/(num1+u)
      wgt <- probs
    }else{
      num1 <- (1-uhat)*svdx$ev
      pj <- num1/(num1+uhat)
      wgt <- 1
    }
    usqrt <- svdx$L[]^2
    num2 <- usqrt%*%pj
    den2 <- usqrt%*%pj^2
    corr <- apply(num2/sqrt(den2)*wgt,1,sum)
    edf <- sum(pj*wgt)
  } else if(method[1] == "qr" && !is.null(corpred)){
    Q <- svdx$Q
    R <- svdx$R
    num1 <- solve(crossprod(R,R)+ diag(umode/(1-uhat),min(n,p),min(n,p)))
    wgt <- 1
    if(n >= p){
      num20 <- tcrossprod(crossprod(t(R),num1),R)
      num2 <- tcrossprod(crossprod(t(Q),num20), Q)
    } else{
      num2 <- tcrossprod(crossprod(R,num1),t(R))
    }
    den2 <- tcrossprod(num2,num2)
    corr <- diag(num2)/diag(sqrt(den2))
    edf <- sum(diag(num2)%*%t(wgt))
  }

  yhat <- yhat+ybar*(!intercept)

  y <- y+ybar*(!intercept)

  intercept  <- intercept*1
  
  res <- list(betahat = betahat,yhat = yhat,sdyhat = sdyhat,sdpred = sdpred,
              varb = varb,sigsqhat = sigsqhat,sigbsqhat = sigbsqhat,u = u,postu = postu,
              uhat = uhat,umode = umode,whichNa = whichNa,phat = phat,delta = delta,edf = edf,corr = corr,
              y = y, intercept = intercept, call = cl, model_frame = m, x = X,svdx=svdx)
  class(res) <- "HDBRR"
  return(res)
}
