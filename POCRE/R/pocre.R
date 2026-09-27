pocre<-function(y, x, lambda=1, x.nop=NA, maxvar=dim(x)[1]/2,
                maxcmp=10, ptype=c('ebtz','ebt','l1','scad','mcp'),
                maxit=100, tol=1e-6, gamma=3.7, pval=FALSE)
{
  y <- as.matrix(y)
  x <- as.matrix(x)
  ptype <- ptype[1]

  retRes <- list()
  retStat <- list()
  eps <- .Machine$double.eps
  n <- dim(x)[1]
  p <- dim(x)[2]
  k <- dim(y)[2]

  if(n!=dim(y)[1]){
    stop("x and y should have the same number of rows!")
  }

  ### Initialization
  retRes$nCmp <- 0
  retRes$mu <- matrix(0,1,k)
  retRes$beta <- matrix(0,n,k)
  retRes$varpi <- matrix(0,p,0)
  retRes$vartheta <- matrix(0,k,0)
  retRes$omega <- matrix(0,p,0)
  retRes$theta <- matrix(0,p,0)
  retRes$bSparse <- 1
  retRes$lambda <- lambda
  retRes$n <- n
  retRes$p <- p
  #retRes$id <- "pocre"

  ### Scale both X and Y
  X <- scale(x,scale=FALSE)
  retRes$xShift <- attr(X,"scaled:center")
  #retRes$xScale <- attr(X,"scaled:scale")
  Y <- scale(y,scale=FALSE)
  retRes$yShift <- attr(Y,"scaled:center")
  #retRes$yScale <- attr(Y,"scaled:scale")

  ### Squentially construct each components
  idxCmp <- 0
  idxNZBeta <- NULL
  omega <- matrix(1,p,1)
  omeganrm <- 1
  #cat("\n\t")
  while((omeganrm>eps) && (idxCmp<maxcmp) && (retRes$bSparse)){
    idxCmp <- idxCmp+1
    tmpRes <- pocre1poc(Y, X, retRes, lambda, tol, maxit,
                        maxcmp, maxvar,ptype, gamma,eps)
    idxNZBeta <- sort(union(idxNZBeta,which(abs(tmpRes$varpi)>eps)))
    if(length(idxNZBeta) > maxvar){
      retRes$bSparse <- 0
    }else if(!isempty(tmpRes$varpi)){  #### varpi value, ok to use is.null
      ###Update omega, theta, varpi, vartheta
      retRes$omega <- cbind(retRes$omega,tmpRes$omega)
      retRes$theta <- cbind(retRes$theta,tmpRes$theta)
      retRes$varpi <- cbind(retRes$varpi,tmpRes$varpi)
      retRes$vartheta <- cbind(retRes$vartheta,tmpRes$vartheta)
      retRes$nCmp <- idxCmp
      X <- tmpRes$X
      cat(".")
    }else{
      omeganrm <- 0
    }
  }
  cat("\n")

  ###Calculate beta
  retRes$beta <- retRes$varpi %*% t(retRes$vartheta)

  ###Calculate mu
  retRes$mu <- retRes$yShift - retRes$xShift %*% retRes$beta;

  ###Calculate sigmae2
  retRes$sigmae2 <- apply(((y-x%*%retRes$beta)-matrix(1,n,1)%*%retRes$mu)^2,2,sum)/(n-retRes$nCmp-1)

  ###Calculate p-values and log-likelihood value if requested
  if(pval==T){
    if( k==1 )
      retStat <- EvalComponentsSig(y,x,retRes,x.nop,eps)
    else
      stop("Calculation of p-values: to be implemented!")

    ###Calculate the log-likelihood function value
    retStat$loglik <- 0
    retStat$effp <- matrix(0,1,k)

    for(j in 1:k){
      tmpX <- cbind(matrix(1,n,1),x[,which(abs(retRes$beta[,j])>eps)])
      tmpRJ <- rref(tmpX)
      tmpRJsum <- apply(tmpRJ,1,sum)
      tmpJ <- which(tmpRJsum!=0)
      retStat$effp[j] <- length(tmpJ)

      if(retRes$bSparse){
        tmpX <- tmpX[,tmpJ]
        yEst <- tmpX%*%as.matrix(lm(y[,j]~tmpX-1)$coefficients)
        retStat$loglik <- retStat$loglik-(log(2*pi)+1+log(sum((y[,j]-yEst)^2)/n))*n/2
      }else
        retStat$loglik <- NA
    }
    retRes <- c(retRes,retStat)
  }

  ###Calculate sigmae2 & R^2
  retRes$rsq <- 1-retRes$sigmae2/apply(y,2,var)
  retRes$nzBeta <- sum(apply(abs(retRes$beta)>eps,2,sum))

  class(retRes) <- 'pocre'
  return(retRes)
}


pocre1poc<-function(y, x, inRes, lambda, tol, maxit,
                    maxcmp, maxvar, ptype, gamma,eps)
{
  ###Initialize retRes
  p <- dim(x)[2]
  retRes <- list()
  retRes$omega <- matrix(0,p,0)
  retRes$theta <- matrix(0,p,0)
  retRes$varpi <- matrix(0,p,0)
  retRes$vartheta <- matrix(0,dim(y)[2],0)

  ###Index of current component
  idxCmp <- length(inRes$varpi)+1
  tmpMat <- t(y) %*% x

  ###Calculate the leading eigenvector of X_j'YY'X_j
  ##tmpU <- svd(tmpMat)$u
  ##tmpS <- svd(tmpMat)$d
  omega <- svd(tmpMat,nu=0,nv=1)$v

  ###Calculate the regularized component vector
  idIter <- 0
  omegapre <- omega
  omegadf <- 1
  omeganrm <- norm(omega,type="2")
  alpha <- t(tmpMat) %*% (tmpMat %*% omega)
  alpha <- alpha/norm(alpha,type="2")
  while((omeganrm>eps) && (omegadf>tol) && (idIter<=maxit)){
    idIter <- idIter + 1
    omegapre <- omega
    omega <- t(tmpMat)%*%(tmpMat%*%alpha)
    if(ptype=="ebt"){
      ###pocre0 via Empirical Bayes Thresholding
      tmpSD <- median(abs(omega)) * lambda/qnorm(0.75,0,1)
      omega <- ebayesthresh(x=omega,sdev=tmpSD, prior = "cauchy")
    } else if(ptype=="ebtz"){
      ###pocre via Empirical Bayes Thresholding
      omega <- omega/norm(omega,type="2")
      omega <- 0.5*log((1+omega)/(1-omega))
      tmpSD <- lambda/sqrt(length(omega)-3)
      mu <- ebayesthresh(x=omega,sdev=tmpSD, prior = "cauchy")
      omega <- 1-2/(exp(2*mu)+1)
    } else if(ptype=="l1"){
      tmpSD <- median(abs(omega)) * lambda/qnorm(0.75,0,1)
      omega <- pmax((abs(omega)-tmpSD*sqrt(2*log(p))),0)*sign(omega)
    } else if(ptype=="scad"){
      tmpSD <- median(abs(omega))*lambda/qnorm(0.75,0,1)
      tmpT <- tmpSD*sqrt(2*log(length(omega)))
      ###|z|<=2*lambda
      tIdx <- which(abs(omega)<=2*tmpT)
      omega[tIdx] <- pmax((abs(omega[tIdx])-tmpT),0)*sign(omega[tIdx])
      ###2*lambda<|z|<=gamma*lambda
      tIdx <- setdiff(which(abs(omega)<=gamma*tmpT),tIdx)
      tmpSO <- pmax((abs(omega[tIdx])-tmpT/(1-1/gamma)),0)
      omega[tIdx] <- tmpSO*sign(omega[tIdx])/(1-1/(gamma-1))
    } else if(ptype=="mcp"){
      tmpSD <- median(abs(omega))*lambda/qnorm(0.75,0,1)
      tmpT <- tmpSD*sqrt(2*log(length(omega)))
      tIdx <- which(abs(omega)<=gamma*tmpT)
      tmpSO <- pmax((abs(omega[tIdx])-tmpT),0)
      omega[tIdx] <- tmpSO*sign(omega[tIdx])/(1-1/gamma)
    } else{
      stop('pocre: The penalization method is not supported!')
    }

    alpha <- t(tmpMat)%*%(tmpMat%*%omega)
    omeganrm <- norm(omega,type="2")
    if(omeganrm>eps){
      alpha <- alpha/norm(alpha,type="2")  ### norm(omega)>eps ==> norm(alpha)>0?
      omegadf <- norm(omegapre-omega,type="2")/omeganrm
    }
  }

  #######
  if(omeganrm>eps){
    ###Update omega, theta, varpi, vartheta
    retRes$omega <- omega/omeganrm
    if(idxCmp==1){
      retRes$varpi <- retRes$omega
    }else{
      retRes$varpi <- varpifromomega(retRes$omega,inRes$omega,inRes$theta)
    }


  eta <- x %*% (retRes$omega)
  retRes$theta <- (t(x)%*%eta)/as.numeric(t(eta)%*%eta)
  retRes$vartheta <- t(y)%*%eta/as.numeric(t(eta)%*%eta)

  ###Update X
  retRes$X <- x-eta%*%t(retRes$theta)
  }
  return(retRes)
}


varpifromomega<-function(omega,omegapre,thetapre)
{
  nCmp = dim(omegapre)[2]
  retVarpi = omega
  for(j in 1:nCmp)
  {
    retVarpi = retVarpi-omegapre[,j] %*% (t(thetapre[,j]) %*% retVarpi)
  }
  return(retVarpi)
}


EvalComponentsSig<-function(y,x,retRes,x.nop,eps)
{
  retStat <- list()
  n <- dim(x)[1]
  p <- dim(x)[2]
  X <- scale(x,scale=FALSE)

  ###Covariates of no interest
  covx <-  NULL
  if(!is.na(x.nop)){
    for(j in 1:retRes$nCmp){
      if(sum(abs(retRes$varpi[x.nop,j]))>eps)
        covx <- cbind(covx,X[,x.nop,drop=F] %*%
                      retRes$varpi[x.nop,j,drop=F])
  }
  ncovx <- dim(covx)[2]

  tidx <- setdiff(1:p,x.nop)
  X <- cbind(covx,X[,tidx] %*% retRes$varpi[tidx,])
  if(!isempty(X)){
    ts <- lm(y~X)
  } else{
    ts <- lm(y~1)
  }
  retStat$pvalue <- summary(ts)$coefficients[-(1:(ncovx+1)),4]

  retStat$seqpv <- retStat$pvalue
  for(j in 1:(retRes$nCmp-1)){
    if(isempty(X)){
      ts <- lm(y~1)
    } else{
      ts <- lm(y~X[,1:(j+ncovx)])
    }
    retStat$seqpv[j] <- (summary(ts)$coefficients)[j+ncovx+1,4]
  }

  retStat$indpv = retStat$seqpv
  for(j in 2:retRes$nCmp){
    if(isempty(X)){
      ts <- lm(y~1)
    } else{
      ts <- lm(y~X[,c(j+ncovx)])
    }
      retStat$indpv[j] <- (summary(ts)$coefficients)[ncovx+2,4]
  }
  return(retStat)
  } else {
    ncovx <- length(covx)
    tidx <- setdiff(1:p,x.nop)
    X <- cbind(covx,X[,tidx] %*% retRes$varpi[tidx,])
    if(!isempty(X)){
      ts <- lm(y~X)
    } else{
      ts <- lm(y~1)
    }
    retStat$pvalue <- (summary(ts)$coefficients)[-(1:(ncovx+1)),4]

    retStat$seqpv <- retStat$pvalue
    if(retRes$nCmp-1>=1){
    for(j in 1:(retRes$nCmp-1)){
      if(isempty(X)){
        ts <- lm(y~1)
      } else{
        ts <- lm(y~X[,1:(j+ncovx)])
      }
      retStat$seqpv[j] <- (summary(ts)$coefficients)[j+ncovx+1,4]
    }

    retStat$indpv = retStat$seqpv
    for(j in 2:retRes$nCmp){
      if(isempty(X)){
        ts <- lm(y~1)
      } else{
        ts <- lm(y~X[,c(j+ncovx)])
      }
        retStat$indpv[j] <- (summary(ts)$coefficients)[ncovx+2,4]
    }
    }
    return(retStat)
  }
}
