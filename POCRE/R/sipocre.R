sipocre<-function(y, x, n.splits=10, delta=0.1, crit=c('ic','cv','fixed'),
                  ptype=c('ebtz','ebt','l1','scad','mcp'), maxvar=dim(x)[1]/2,
                  msc=NA, maxit=100, maxcmp=50, gamma=3.7, tol=1e-6,
                  n.folds=10, lambda=1, n.train=round(nrow(x)/2))
{
  y <- as.matrix(y)
  x <- as.matrix(x)
  crit <- crit[1]
  ptype <- ptype[1]

  retRes <- list()
  if(crit=="ic"){
    if(is.na(msc)) msc <- c(seq(0,1,0.05),2,3)
  }
  else if(crit=="fixed"){
    if(is.na(msc)) msc = 1
  }

  n <- dim(x)[1]
  p <- dim(x)[2]
  k <- dim(y)[2]
  if(n!=dim(y)[1]){
    stop("x and y should have the same number of rows!")
  }

  ### Scale both X and Y
  X <- scale(x)
  #retRes$xShift <- attr(X,"scaled:center")
  sdX <- attr(X,"scaled:scale")
  Y <- scale(y,scale=FALSE)
  #retRes$yShift <- attr(Y,"scaled:center")
  sdY <- attr(Y,"scaled:scale")

  set.seed(999)
  if(crit=="ic"){
    nMSC <- length(msc)
  } else {
    nMSC <-1
  }

  retRes$cpv <- matrix(1,p,nMSC)
  retRes$xpv <- matrix(1,p,nMSC)
  mcpv <- array(1,dim = c(p,n.splits,nMSC))
  mxpv <- array(1,dim = c(p,n.splits,nMSC))
  for(j in 1:n.splits){
    print(paste("   Random Split: ", j))
    idxTmp <- sample(n)
    idxTr <- idxTmp[1:n.train]
    idxTs <- idxTmp[-(1:n.train)]

    if(crit=="ic"){
      res <- pocrepath(as.matrix(Y[idxTr,]),X[idxTr,],delta,maxvar=maxvar,NA,
                       maxcmp,ptype,maxit=maxit,tol=tol,gamma=gamma,pval=FALSE)
      for(kk in 1:nMSC){
        tmpRes <- selectmodel(res,msc[kk])
        temp <- CalcPV4TestData(y[idxTs,],x[idxTs,],tmpRes)
        mcpv[,j,kk] <- temp[[1]]   ##check for consistent
        mxpv[,j,kk] <- temp[[2]]
      }
    } else if(crit=="cv"){
      tp <- cvpocre(as.matrix(Y[idxTr,]),X[idxTr,],n.folds,delta,
                    maxvar,ptype,maxit,maxcmp,gamma,tol=tol)

      tmpRes <- pocre(as.matrix(Y[idxTr,]),X[idxTr,],tp,NA,maxvar,maxcmp,
                      ptype,maxit,tol,gamma,pval=FALSE)

      temp <- CalcPV4TestData(y[idxTs,],x[idxTs,],tmpRes)
      mcpv[,j,nMSC] <- temp[[1]] ##consistent
      mxpv[,j,nMSC] <- temp[[2]]
    } else if(crit=="fixed"){
      tmpRes <- pocre(as.matrix(Y[idxTr,]),X[idxTr,],lambda,NA,maxvar,maxcmp,
                      ptype,maxit,tol,gamma)
      temp <- CalcPV4TestData(as.matrix(Y[idxTr,]),X[idxTr,],tmpRes$retRes)
      mcpv[,j,nMSC] <- temp[[1]]
      mxpv[,j,nMSC] <- temp[[2]]
    }
  }

  #Pool p-values
  if(crit=="ic"){
    for(kk in 1:nMSC){
      retRes$cpv[,kk] <- poolpvalues(mcpv[,,kk])
      retRes$xpv[,kk] <- poolpvalues(mxpv[,,kk])
    }
  } else{
    retRes$cpv <- poolpvalues(mcpv)
    retRes$xpv <- poolpvalues(mxpv)
  }

  return(retRes)
}

CalcPV4TestData <- function(y, x, inRes)
{
  eps <- .Machine$double.eps
  n <- dim(x)[1]
  p <- dim(x)[2]
  retXPV <- matrix(1, p, 1)

  ##Component-based p-values:
  #X <- center_apply(x) %*% inRes$varpi
  X <- scale(x,scale=FALSE) %*% inRes$varpi
  ts <- summary(lm(y~X))
  tmpPV <- ts$coefficients[-1,4]

  tmpIPV <- matrix(1, p, inRes$nCmp)

  for(j in 1:inRes$nCmp){
    tmpIPV[which(abs(inRes$varpi[,j])>eps),j] <- tmpPV[j]
  }
  # How to summarize tmpIPV for retCPV? Any theoretical support?
  if(inRes$nCmp>0){
    retCPV <- apply(tmpIPV,1,min)
  } else{
    retCPV <- matrix(1,p,1)
  }

  #traditional pvalues
  xIdx <- which(abs(inRes$beta)>eps)
  tmpR <- rref(cbind(matrix(1,n,1),x[,xIdx]))
  tmpRJsum <- apply(tmpR,1,sum)
  tmpJ <- which(tmpRJsum!=0)
  uxIdx = xIdx[tmpJ[-length(tmpJ)]]
  ts <- summary(lm(y~x[,uxIdx]))
  retXPV[uxIdx] <- ts$coefficients[-1,4]
  #Q: How to assign p-values to excluded multicollinear covariates
  for( j in setdiff(1:length(xIdx), tmpJ[-length(tmpJ)]) )
    retXPV[j] <- min(ts$coefficients[which(tmpR[,j]!=0)])
  return(list(retCPV,retXPV))
}


poolpvalues <- function(inMPV)
{
  p <- dim(inMPV)[1]
  tmppv <- matrix(0, p, 19)
  for(k in 1:19){
    gamma <- k*0.05
    tmppv[,k] <- pmin(apply(inMPV,1,function(x) quantile(x,gamma,type=5,na.rm=T))/gamma,matrix(1,p,1))
  }
  retPV <- pmin((1-log(0.05))*apply(tmppv,1,min),ones(p,1))
  return(retPV)
}
