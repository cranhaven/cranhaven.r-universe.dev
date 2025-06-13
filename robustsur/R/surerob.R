surerob <- function(formula, data,  
  control=lmrob.control(), ...) {
  results <- list()
  results$eq <- list()
  if (!is.list(formula))
    stop("'surerob' is used only for more than one equation. For a single equation model used function 'lmrob' in package 'robustbase'")
  nEq <- length(formula)
  if (!is.data.frame(data)) {
    if (is.list(data)) {
      if (length(data)!=nEq)
        stop("if 'data' is a 'list', it must have the same length of the list 'formula'")
      n <- NROW(data[[1]])
    } else {
        stop("'data' must be a 'list' or a 'data.frame'")  
    }
  } else {
      n <- NROW(data)
  }
  if (is.null(names(formula))) {
    eqnLabels <- paste("eq", c(1:nEq), sep = "")
  } else {
    eqnLabels <- names(formula)
    if (sum(regexpr(" |_", eqnLabels) != -1) > 0) {
      stop("equation labels may not contain blanks (' ') or underscores ('_')")
    }
  }
  
  # Get the original call
  results$call <- match.call()
  ## First step
  # list of model matrices for the individual equations
  X <- list()
  # list of model matrices multiplied for the square root of the weights for 
  # the individual equations
  wX <- list()
  # list of models evaluated at the individual equations
  SingleEq <- list()
  # vectors of dependent variables in each equation (and multiplied by the square root of the weights)
  wY <- Y <- vector(length=0)
  # vector of weights
  w <- vector(length=0)
  # number of variables in each equation
  ncoef <- numeric(nEq)
  # number of observations in each equation
  nobs <- numeric(nEq)
  nomi <- vector(length=0)
  y <- matrix(NA, nrow=n, ncol=nEq)
  for (i in 1:nEq) {
    if (is.data.frame(data))
      SingleEq[[i]] <- lmrob(formula=formula[[i]], data=data, y=TRUE, control=control)  
    else
      SingleEq[[i]] <- lmrob(formula=formula[[i]], data=data[[i]], y=TRUE, control=control)
    nomi <- c(nomi, paste(eqnLabels[i], names(SingleEq[[i]]$coefficients), sep="_"))
    Y <- c(Y,SingleEq[[i]]$y)
    y[,i] <- SingleEq[[i]]$y
    X[[i]] <- SingleEq[[i]]$x
    ncoef[i] <- ncol(X[[i]])
    nobs[i] <- length(SingleEq[[i]]$y)
    w <- cbind(w, SingleEq[[i]]$rweights)
    wY <- c(wY, sqrt(SingleEq[[i]]$rweights)*SingleEq[[i]]$y)
    wX[[i]] <- diag(sqrt(SingleEq[[i]]$rweights))%*%X[[i]]
    #w2X[[i]] <- diag(results$eq[[i]]$rweights)%*%X[[i]]
  }
  X <- bdiag(X)
  wX <- bdiag(wX)
  #w2X <- bdiag(w2X)
  ncoefTot <- sum(ncoef)
  nobsTot <- sum(nobs)
  df <- nobs - ncoef    # degress of freedom of each equation
  
  # residuals
  resid <- matrix(0, nrow=n, ncol=nEq)
  for (i in 1:nEq) {
    resid[,i] <- SingleEq[[i]]$residuals 
  }
  # residuals covariance
  S <- TSGS(resid, ...)@S

  ## Second step
  eS <- eigenkronecker(S, n)
  d <- eS$values  
  if (any(d <= 0)) 
    stop("'S' is not positive definite")
  v <- eS$vector
  A <- d^(-0.5)*v
  Ainv <- t(d^0.5*v)
  fit <- lm.fit(as.matrix(A %*% wX), A %*% wY)
  beta <- list()
  cp <- c(0,cumsum(ncoef))
  for (i in 2:(nEq+1)) {
    beta[[i-1]] <- fit$coefficients[(cp[i-1]+1):cp[i]]
  }
  
  xwx <- solve(t(as.matrix(wX))%*%as.matrix(wX))
  ### F <- drop(Ainv %*% fit$fitted.values)
  F <- drop(X%*%fit$coefficients)
  fitted.values <- matrix(F, nrow=n)
  ### R <- drop(Ainv %*% fit$residuals)
  R <- Y - F
  residf <- matrix(R, ncol=nEq)
  Sfinal <- TSGS(residf, ...)
  coefCov <- as.matrix(xwx%*%t(as.matrix(wX))%*%kronecker(Sfinal@S, diag(n))%*%as.matrix(wX)%*%xwx)
  # equation results
  for (i in 1:nEq) {
    results$eq[[i]] <- list()
    results$eq[[i]]$eqnNo <- i
    results$eq[[i]]$eqnLabel <- eqnLabels[i]
    results$eq[[i]]$call <- SingleEq[[i]]$call
    results$eq[[i]]$na.action <- SingleEq[[i]]$na.action
    results$eq[[i]]$iter <- SingleEq[[i]]$iter
    results$eq[[i]]$terms <- SingleEq[[i]]$terms
    results$eq[[i]]$converged <- SingleEq[[i]]$converged
    results$eq[[i]]$qr <- SingleEq[[i]]$qr
    results$eq[[i]]$scale <- SingleEq[[i]]$scale
    results$eq[[i]]$control <- SingleEq[[i]]$control
    results$eq[[i]]$control$method <- "M-SMDM-M"
    results$eq[[i]]$method <- "robustsur"
    results$eq[[i]]$residuals <- residf[,i]
    results$eq[[i]]$ssr <- sum(w[,i] * residf[,i]^2)
    results$eq[[i]]$coefficients <- beta[[i]]
    names(results$eq[[i]]$coefficients) <- names(SingleEq[[i]]$coefficients)
    results$eq[[i]]$cov <- as.matrix( coefCov[ (cp[i]+1):cp[i+1],(cp[i]+1):cp[i+1] ] )
    results$eq[[i]]$fitted.values <- fitted.values[,i]
    results$eq[[i]]$rank <- ncoef[i]
    results$eq[[i]]$rank.sys <- ncoefTot
    results$eq[[i]]$nCoef.sys <- ncoefTot
    results$eq[[i]]$df.residuals <-df[i]
    results$eq[[i]]$dfresidual.sys <- nobsTot - ncoefTot
    results$eq[[i]]$y <- SingleEq[[i]]$y
    results$eq[[i]]$x <- SingleEq[[i]]$x
    results$eq[[i]]$rweights <- w[,i]
    class(results$eq[[i]]) <- "lmrob"
  }
  results$method <- "Robust SUR"
  results$rank <- ncoefTot
####  results$beta <- beta
  names(fit$coefficients) <- nomi
  results$coefficients <- fit$coefficients
  results$fitted.values <- F
  results$residuals <- R
  results$imp.residuals <- Sfinal@ximp
  results$residCovEst <- S ###TSGS(resid)@S
  results$residCov <- Sfinal@S
  colnames(results$residCov) <- eqnLabels
  rownames(results$residCov) <- eqnLabels
  results$coefCov <- coefCov
  results$rweights <- w
  results$TSGS <- Sfinal
  results$control <- control
  results$df.residual <- nobsTot - ncoefTot
  results$y <- Y
  results$x <- X
##  results$case_weights <- Sfinal@weights
##  results$case_weightsp <- Sfinal@weightsp
  class(results) <- c("surerob", "systemfit")
  results
}

if (FALSE) {
library(mvtnorm)
m <- 2
p <- 2
n <- 100
rho <- 0.8
V <- matrix(rho, m, m)
diag(V) <- 1
e <- rmvnorm(n=n, rep(0,m), V)
dati <- formule <- list()
truebeta <- list()  
for (i in 1:m) {
  x <- matrix(rnorm(n*p), nrow=n)
  beta <- truebeta[[i]] <- rcauchy(p)
  truebeta[[i]] <- c(0,truebeta[[i]])
  y <- x%*%beta+e[,i]
  dati[[i]] <- data.frame(x=x, y=y)
  colnames(dati[[i]]) <- c("x1","x2","y")
  formule[[i]] <- y ~ x1 + x2
}
 
rsur <- surerob(formule, dati)  

}  