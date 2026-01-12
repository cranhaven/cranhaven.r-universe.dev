## All the functions in the following are from the Rpackage ITRSelect
## estimated concordance function
visa.Cest <- function(beta, X, A, Y, pi.est, h.est){

  concor <- as.vector((A-pi.est)*(Y-h.est)/(pi.est*(1-pi.est)))
  Api <- A/pi.est
  concorApi <- as.matrix(outer(concor, Api, "*"))
  concorApi <- concorApi-t(concorApi)

  Xbeta <- as.vector(X%*%beta)
  IXbXb <- (outer(Xbeta, Xbeta, "-")>0)

  return (mean(concorApi*IXbXb))
}

## CIC criteria
visa.CIC <- function(beta, X, A, Y, pi.est, h.est, kap=1){

  n <- length(Y)
  p <- dim(X)[2]
  d <- sum(abs(beta)>1e-15)

  return (n*visa.Cest(beta, X, A, Y, pi.est, h.est)-d*log(p)*log(n, 10)*log(log(n, 10))/kap)
}

## VIC criteria
visa.VIC <- function(beta, X, A, Y, pi.est, h.est, kap=4){

  n <- length(Y)
  p <- dim(X)[2]
  gA <- A*((cbind(1,X)%*%beta)>0)+(1-A)*((cbind(1,X)%*%beta)<=0)
  Api <- A*pi.est+(1-A)*(1-pi.est)
  gApi <- gA/Api
  d <- sum(abs(beta[2:(p+1)])>1e-15)

  return (n*mean(gApi*Y-(gApi-1)*(h.est+pmax(cbind(1,X)%*%beta,0)))-d*n^(1/3)*(log(p))^(2/3)*log(log(n))/kap)
}

## BIC criteria
visa.BIC <- function(beta, X, A, Y, pi.est, h.est, kap=1){

  d <- sum(abs(beta)>1e-15)
  n <- length(Y)
  p <- dim(X)[2]
  X0 <- cbind(1, X)
  BIC0 <- mean(((A-pi.est)*(Y-h.est-A*(X0%*%beta)))^2)
  BIC0 <- -n*log(BIC0) - d*(log(n)+log(p+1))/kap

  return (BIC0)
}

## the Dantzig selector
visa.IC.Dantzig <- function(X, Y, A, pi.est, h.est, lambda.list, IC="CIC", kap, refit=TRUE){

  p <- dim(as.matrix(X))[2]
  lambdan <- length(lambda.list)

  # all the coefficients
  beta.all <- matrix(0, lambdan, p+1)

  # all the criteria
  obj <- rep(0, lambdan)

  ## standardization
  center <- colMeans(X)
  std <- sqrt(apply(X, 2, var))
  ## estimation
  X0 <- cbind(1, t((t(X)-center)/std))
  n <- length(A)

  #parameters
  flp <- c(1,rep(0,2*(p+1))) #model parameters: t,beta+,beta-
  #condition on estimating equation
  Ynew <- colSums(X0*as.vector((A-pi.est)*(Y-h.est)))
  Xnew <- crossprod(X0*as.vector(A-pi.est),(X0*as.vector(A)))
  #constraint matrix
  Alp <- Matrix(rbind(cbind(-1,-Xnew,Xnew),cbind(-1,Xnew,-Xnew),c(0,rep(1,p+1),rep(1,p+1))))

  #cat("\n")
  for (i in 1:lambdan){
    #constraint upper bound
    blp <- c(-Ynew,Ynew,lambda.list[i])

    #lower bound on variables
    dir <- rep("<=", 2*(p+1)+1)
    #lower bound on variables
    bounds <- list(lower = list(ind = 2:(2*(p+1)+1), val = rep(0, 2*(p+1))))
    result <- Rglpk_solve_LP(obj=flp, mat=Alp, dir=dir, rhs=blp, bounds=bounds, max=FALSE)


    beta.est <- result$solution[2:(p+2)]-result$solution[(p+3):(2*p+3)]
    beta.est[2:(p+1)] <- beta.est[2:(p+1)]/std
    beta.est[1] <- beta.est[1]-crossprod(center, beta.est[2:(p+1)])

    ###########################################
    #Double with estimating equation
    ###########################################
    if (refit&&sum(beta.est[2:(p+1)]!=0)>0&&sum(beta.est!=0)<(n/log(n))){
      index <- (1:p)[beta.est[2:(p+1)]!=0]
      X.refit <- as.matrix(cbind(1,X[, index]))
      Ynew0 <- colSums(X.refit*as.vector((A-pi.est)*(Y-h.est)))
      Xnew0 <- crossprod((X.refit*as.vector(A-pi.est)),(X.refit*as.vector(A)))
      beta.refit <- solve(Xnew0, Ynew0)
      beta.est[c(1, index+1)] <- beta.refit
    }

    beta.all[i, ] <- beta.est
    if (IC=="CIC")
      obj[i] <- visa.CIC(beta.est[2:(p+1)], X, A, Y, pi.est, h.est, kap=kap)
    else if (IC=="BIC")
      obj[i] <- visa.BIC(beta.est, X, A, Y, pi.est, h.est, kap=kap)
    else
      obj[i] <- visa.VIC(beta.est, X, A, Y, pi.est, h.est, kap=kap)

  }

  return (as.vector(beta.all[which.max(obj), ]))
}
