gps<-function(y, x, family="binomial", bc.method="optimal", x.include=NULL, 
              weights=NULL, maxcmp=10, maxvar=NULL, tol = 1e-6, maxit = 100)
{
  # Organize inputs
  ## (1) Allocate inputs
  y <- as.matrix(y)
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y)
  eps <- .Machine$double.eps
  if(is.null(maxvar)) maxvar <- min(n,p)/2
  
  ## (2) Decide family
  if( is.character(family) )
    family <- get(family, mode = "function", envir = parent.frame())()
  if( is.function(family) )
    family <- family()
  if( is.null(family$family)|
      (! family$family %in% c('binomial', 'gaussian','poisson'))){
    stop(paste("The family ", family$family, " is not supported ..."))
  }
  ## (3) Check the format of input
  if(n!=dim(as.matrix(y))[1])
    stop("     gpocre: X and Y should have the same number of rows!")
  
  # Container for output
  retRes <- list()
  # Initial values
  retRes$beta <- matrix(0,p,q)
  if( is.null(weights) ){
    retRes$weights <- matrix(1,n,1)/n
  }else{
    retRes$weights <- weights/sum(weights)
  }
  retRes$nCmp <- 0
  retRes$mu <- family$linkfun(matrix(apply(y,2,mean),nrow=1))
  retRes$varpi <- matrix(0,p,0)
  retRes$vartheta <- matrix(0,dim(y)[2],0)
  retRes$xomega <- matrix(0,n,0)
  retRes$omega <- matrix(0,p,0)
  retRes$theta <- matrix(0,p,0)
  
  #Define indicator vectors for x.include (idcE) & retSIdx (idcS)
  idcE <- matrix(0,p,1)
  if(!is.null(x.include)){
    idcE[x.include] <- 1
  }
  idcS <- matrix(0,p,1)
  # Centralize X appropriately according to W:
  mX <- colMeans(x*as.vector(retRes$weights))
  tmpX <- x-matrix(1,n,1)%*%mX
  
  # Construct all components
  idxCmp <- 0
  omeganrm <- 1
  nSele <- round(maxvar/maxcmp)
  zeta <- biascorrectioncoeff(tmpX,family,retRes$weights,bc.method)
  cat(" Screening variables ")
  while( (sum(idcS)<maxvar) && (idxCmp<maxcmp) ){
    idxCmp <- idxCmp+1
    if( idxCmp==maxcmp ){
      nSele <- maxvar - nSele*(maxcmp-1)
    }
    # Construct the component
    tmpRes <- gpocrescreen1poc(y=y, x=tmpX, family=family, preRes=retRes,
                               inIdc=idcE+idcS, inNx=nSele, zeta=zeta, tol=tol,
                               maxit=maxit, maxvar=maxvar)
    omeganrm <- fnorm(tmpRes$omega)
    idcS <- idcS+tmpRes$idcS
    retRes$mu <- tmpRes$mu
    retRes$omega <- cbind(retRes$omega,tmpRes$omega)
    retRes$theta <- cbind(retRes$theta,tmpRes$theta)
    retRes$varpi <- cbind(retRes$varpi,tmpRes$varpi)
    retRes$vartheta <- tmpRes$vartheta
    retRes$xomega <- cbind(retRes$xomega,tmpX%*%tmpRes$omega)
    retRes$nCmp <- idxCmp
    
    tmpX = tmpRes$X
    cat(".")
  }
  
  cat("\n")
  ###Calculate beta
  #retRes$beta <- retRes$varpi %*% t(retRes$vartheta)
  
  ###Calculate mu
  #mX <- colMeans(x*as.vector(retRes$weights))
  #tmpX <- x-matrix(1,n,1)%*%mX
  #retRes$mu <- retRes$mu - mX %*% retRes$beta
  retSIdx <- which(idcS!=0)
  #retX <- x[,c(x.include,retSIdx)]
  #retAll <- list(retX=retX, retSIdx=retSIdx, retRes=retRes)
  #class(retAll) <- 'gps'
  
  return(retSIdx)
}



fnorm <- function(x)
{
  sqrt(sum(x^2))
}



robustize <- function (Z)
{
  Z <- as.matrix(Z)
  rmean <- mean(Z, trim=0.25)
  rstd <- median(abs(Z-rmean))/pnorm(0.75)
  cutoff <- qnorm(1-0.01/nrow(Z))*rstd
  retZ <- Z
  retZ[retZ<(rmean-cutoff)] <- rmean-cutoff
  retZ[retZ>(rmean+cutoff)] <- rmean+cutoff
  
  return(retZ)
}



biascorrectioncoeff <- function(X,family,weights,bc.method)
{
  n <- nrow(X)
  p <- ncol(X)
  
  if( (family$family=="binomial")|(family$family=="multinom") ) {
    switch(bc.method,
           "none" = {zeta <- matrix(0,n,1)},
           "approx"={zeta <- 1-weights},
           "optimal"={
             tmpM <- X*(sqrt(weights)%*%matrix(1,1,p))
             tmpU <- svd(tmpM,nv=0)$u
             zeta <- diag(tmpU %*% t(tmpU))},
           { zeta <- matrix(0,n,1)
             warning(paste("The bias correction method ",bc.method," is not supported ..."))})
  }else{
    zeta <- 1 - weights
  }
  
  return(zeta)
}



gpocrescreenslev <- function(z, x, inIdc, inNx, tol, maxit)
{
  eps <- .Machine$double.eps
  retLEV <- NULL
  
  tmpMat <- t(z) %*% x
  
  ###Calculate the leading eigenvector of X_j'YY'X_j
  ##tmpU <- svd(tmpMat)$u
  ##tmpS <- svd(tmpMat)$d
  omega <- svd(tmpMat,nu=0,nv=1)$v
  
  ###Calculate the regularized component vector
  idIter <- 0
  omegapre <- omega
  omegadf <- 1
  omeganrm <- sqrt(sum(omega^2))
  if(max(tmpMat)>10^150){
    tmpMat <- tmpMat/max(tmpMat)
  }
  alpha <- t(tmpMat)%*%(tmpMat%*%omega)
  alpha <- alpha/sqrt(sum(alpha^2))
  
  while((omeganrm>eps) && (omegadf>tol) && (idIter<=maxit)){
    idIter <- idIter + 1
    omegapre <- omega
    omega <- t(tmpMat)%*%(tmpMat%*%alpha)
    tmpCutoff <- sort(abs(omega[inIdc==0, ]),decreasing = T)[inNx+1]
    omega <- pmax(abs(omega)-tmpCutoff,0)*sign(omega)
    alpha <- t(tmpMat)%*%(tmpMat%*%omega)
    omeganrm <- norm(omega,type="2")
    if(omeganrm>eps){
      alpha <- alpha/norm(alpha,type="2")  ### norm(omega)>eps ==> norm(alpha)>0?
      omegadf <- norm(omegapre-omega,type="2")/omeganrm
    }
  }
  
  retLEV <- ifelse(rep(omeganrm, length(omega))>eps, omega/omeganrm, omega)
  return(retLEV)
}



gpocrescreen1poc <- function(y, x, family, preRes, inIdc, inNx, zeta, tol, 
                             maxit, maxvar)
{
  eps <- .Machine$double.eps
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y)
  if(n!=nrow(y))
    stop('gPOCRE: X and Y should have the same number of rows!');
  
  idxCmp <- preRes$nCmp+1
  ##Initial Values
  retRes <- list()
  retRes$mu <- preRes$mu
  #  if( bLatent ){
  retRes$vartheta <- cbind(preRes$vartheta,0)
  # }else{
  #  retRes$vartheta <- cbind(preRes$vartheta,matrix(0,q,1))
  #}
  retRes$omega <- matrix(0,p,1)
  retRes$varpi <- matrix(0,p,1)
  retRes$idcS <- matrix(0,p,1)
  
  tmpDen <- matrix(1,idxCmp,1)
  if( idxCmp>1 ){
    for(j in 1:(idxCmp-1)){
      len <- length(preRes$xomega[,j])
      tmpDen[j] <- t(preRes$xomega[,j])%*%(preRes$xomega[,j]*preRes$weights)
    }
  }
  
  omegaPre <- retRes$omega
  omegaDiff <-1
  idIter <-0
  while((omegaDiff>tol)&(idIter<maxit)){
    
    idIter <- idIter+1
    omegaPre <- retRes$omega
    ##update eta
    
    # eta1 <- cbind(preRes$xomega, x%*%retRes$omega)%*%t(retRes$vartheta)
    eta <- cbind(preRes$xomega, x%*%retRes$omega)%*%t(retRes$vartheta)+ rep(1, n)%*%retRes$mu
    
    ##update Z
    if( q==1 ) {
      Z <- eta +(y+zeta/2-(1+zeta)*family$linkinv(as.matrix(eta)))/((1+zeta)*family$mu.eta(eta))
      
      # Robust values of Z
      Z <- robustize(Z)   # May do it later???
    } else { # Use eta.mu defined in multinomial & no bias correction!
      
      calcz <- function(etai,yi){etai+t((yi+0.99/2)/(1+0.5*(q+1)*0.99)-family$linkinv(etai))%*%as.matrix(family$mu.eta(family$linkinv(etai)))}
      
      Z <- t(mapply(calcz,as.list(as.data.frame(t(eta))), as.list(as.data.frame(t(y)))))
      
      # Robust values of Z
      Z <- apply(Z,2,robustize)
      # Robust values of Z
      #Z <- apply(Z,1,robustize)
    }

    # Weighted Z
    Z <- Z*(preRes$weights%*%t(rep(1,q)))
    
    # Update mu
    retRes$mu <- as.matrix(t(apply(Z,2,sum)))
    retRes$omega <- gpocrescreenslev(z=Z, x=x, inIdc=inIdc, inNx=inNx, tol=tol, maxit=maxit)

    if( norm(retRes$omega,'2')<eps ){
      omegaDiff <- 0;
      retRes$vartheta[,idxCmp] = matrix(0,q,1);
    } else {
      ## Update all vartheta
      if(idxCmp>1){
        for (j in 1:(idxCmp-1)){
          retRes$vartheta[,j] = t(Z)%*%preRes$xomega[,j]/tmpDen[j];
        }
      }
      tmpDen[idxCmp] <- t(retRes$omega)%*%t(x)%*%(x*matrix(preRes$weights,nrow=dim(x)[1],ncol=dim(x)[2]))%*%retRes$omega;
      
      if( tmpDen[idxCmp] < eps ){
        retRes$vartheta[,idxCmp] = matrix(0,q,1)
      }else{
        retRes$vartheta[,idxCmp] = t(Z)%*%x%*%retRes$omega/tmpDen[idxCmp]
      }
      omegaDiff <- norm(omegaPre-retRes$omega,'2');
    }
  }
  ##Update X
  retRes$theta <- t(x)%*%(x*matrix(preRes$weights,nrow=nrow(x),ncol=ncol(x)))%*%retRes$omega/tmpDen[idxCmp]
  retRes$X <- x-x%*%retRes$omega%*%t(retRes$theta)
  
  ##Calculate varpi
  retRes$varpi <- retRes$omega
  if(idxCmp>1){
    for(j in ((idxCmp-1):1))
    {
      retRes$varpi <- retRes$varpi-preRes$omega[,j]%*%(t(preRes$theta[,j])%*%retRes$varpi)
    }
  }
  retRes$idcS <- pmax((abs(retRes$omega)>eps)-inIdc,0)
  
  return(retRes)
}