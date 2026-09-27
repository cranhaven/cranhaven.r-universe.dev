pocrescreen<-function(y, x, maxvar=nrow(x), maxcmp=5, x.include=NULL,
                      tol=1e-6, maxit=100)
{
  y <- as.matrix(y)
  x <- as.matrix(x)

  eps <- .Machine$double.eps
  retRes <- list()
  n <- dim(x)[1]
  p <- dim(x)[2]
  k <- dim(y)[2]
  if(n!=dim(y)[1])
    stop("x and y should have the same number of rows!")
  if(is.null(maxvar)){
    maxvar <- min(n/log(n), p/2)
  }
  maxvar <- round(maxvar)

  retRes$nCmp <- 0
  retRes$mu <- matrix(0,1,k)
  retRes$beta <- matrix(0,n,k)
  retRes$varpi <- matrix(0,p,0)
  retRes$vartheta <- matrix(0,k,0)
  retRes$omega <- matrix(0,p,0)
  retRes$theta <- matrix(0,p,0)
  #retRes$bSparse <- 1

  #Define indicator vectors for x.include (idcE) & retSIdx (idcS)
  idcE <- matrix(0,p,1)
  if(!is.null(x.include)){
    idcE[x.include] <- 1
  }
  idcS <- matrix(0,p,1)

  ### Scale both X and Y
  X <- scale(x,scale=FALSE)
  retRes$xShift <- attr(X,"scaled:center")
  #retRes$xScale <- attr(X,"scaled:scale")
  Y <- scale(y,scale=FALSE)
  retRes$yShift <- attr(Y,"scaled:center")
  #retRes$yScale <- attr(Y,"scaled:scale")

  #Squentially construct each components
  idxCmp <- 0
  omega <- matrix(1,p,1)
  omeganrm <- 1
  nSele <- round(maxvar/maxcmp)
  cat(" Screening variables ")
  while( (sum(idcS)<maxvar) && (idxCmp<maxcmp) ){
    idxCmp <- idxCmp+1
    if( idxCmp==maxcmp ){
      nSele <- maxvar - nSele*(maxcmp-1)
    }
    tmpRes <- pocrescreen1poc(Y, X, retRes, idcE+idcS, nSele, tol, maxit, maxcmp, maxvar)
    idcS <- idcS+tmpRes$idcS
    #Update omega, theta, varpi, vartheta
    retRes$omega <- cbind(retRes$omega,tmpRes$omega)
    retRes$theta <- cbind(retRes$theta,tmpRes$theta)
    retRes$varpi <- cbind(retRes$varpi,tmpRes$varpi)
    retRes$vartheta <- cbind(retRes$vartheta,tmpRes$vartheta)
    retRes$nCmp <- idxCmp

    X = tmpRes$X
    cat(".")
  }
  cat("\n")
  ###Calculate beta
  #retRes$beta <- retRes$varpi %*% t(retRes$vartheta)
  ###Calculate mu
  #retRes$mu <- retRes$yShift - retRes$xShift %*% retRes$beta
  ###Calculate sigmae2
  #retRes$sigmae2 <- sum(((y-x%*%retRes$beta)-as.numeric(retRes$mu))^2)/(n-retRes$nCmp)

  retSIdx <- which(idcS!=0)
  #retX <- x[,c(x.include,retSIdx)]
  #return(list(retX=retX, retSIdx=retSIdx, retRes=retRes))
  return(retSIdx)
}



pocrescreen1poc<-function(y, x, inRes, inIdc, inNX, tol, maxit, maxcmp, maxvar)
{
  eps <- .Machine$double.eps

  p <- dim(x)[2]
  retRes <- list()
  retRes$omega <- matrix(0,p,0)
  retRes$theta <- matrix(0,p,0)
  retRes$varpi <- matrix(0,p,0)
  retRes$vartheta <- matrix(0,dim(y)[2],0)
  retRes$idcS <- matrix(0,p,1)

  idxCmp <- dim(inRes$varpi)[2] + 1   #Index of current component
  tmpMat <- t(y) %*% x

  ###Calculate the leading eigenvector of X_j'YY'X_j
  omega <- svd(tmpMat,nu=0,nv=1)$v

  ###Calculate the regularized component vector
  idIter <- 0
  omegapre <- omega
  omegadf <- 1
  omeganrm <- norm(omega,type="2")
  alpha <- t(tmpMat) %*% (tmpMat %*% omega)
  alpha <- alpha/norm(alpha,type="2")
  while( (omeganrm>eps) && (omegadf>tol) && (idIter<=maxit) ){
    idIter <- idIter + 1
    omegapre <- omega
    omega <- t(tmpMat)%*%(tmpMat%*%alpha)

    ###Hard Threshold or Soft Threshold?
    ## can use nth from package fastR to make it faster?
    tmpCutoff <- sort(abs(omega[which(inIdc!=1)]), decreasing = T)[inNX+1]

    omega <- pmax((abs(omega)-tmpCutoff),0) * sign(omega)

    alpha <- t(tmpMat) %*% (tmpMat %*% omega)
    omeganrm <- norm(omega,type="2")

    if( omeganrm>eps ){
      alpha <- alpha/norm(alpha,type="2")
      ###norm(omega)>eps ==> norm(alpha)>0?
      omegadf <- norm(omegapre-omega,type="2")/omeganrm
    }
  }


  ###Update omega, theta, varpi, vartheta
  retRes$omega <- omega/omeganrm
  if(idxCmp==1){
    retRes$varpi <- retRes$omega
  } else {
    retRes$varpi <- varpifromomega(retRes$omega,inRes$omega,inRes$theta)
  }

  eta <- x %*% (retRes$omega)
  retRes$theta <- (t(x)%*%eta)/as.numeric(t(eta)%*%eta)
  retRes$vartheta <- t(y)%*%eta/as.numeric(t(eta)%*%eta)

  ###Update X
  retRes$X <- x-eta%*%t(retRes$theta)
  retRes$idcS <- pmax((abs(omega)>eps)-inIdc,0)
  return(retRes)
}

varpifromomega<-function(omega,omegapre,thetapre)
{
  nCmp <- dim(omegapre)[2]
  retVarpi <- omega
  for(j in 1:nCmp){
    retVarpi <- retVarpi - omegapre[,j] %*% (t(thetapre[,j]) %*% retVarpi)
  }
  return(retVarpi)
}
