surerob2 <- function(formula, data, alpha=0.01, ...) {
  m <- length(formula)
  n <- NROW(data[[1]])
  ## First step
  singleEq <- X <- wX <- list()
  p <- wY <- Y <- w <- vector(length=0)
  # residuals
  resid <- matrix(0, nrow=n, ncol=m)
  for (i in 1:m) {
    mf <- lm(formula[[i]], data=data[[i]], method="model.frame")
    y <- model.response(mf)
    mt <- attr(mf, "terms")
    conintercetta <- FALSE
    if (attr(mt, "intercept")==1) {
      conintercetta <- TRUE
    }
    attr(mt, "intercept") <- 0
    x <- model.matrix(mt, mf)
    singleEq[[i]] <- robreg3S(y=y, x=x, alpha=alpha, ...)
    Y <- c(Y,y)
    X[[i]] <- singleEq[[i]]$ximpute
    if (conintercetta)
      X[[i]] <- cbind(1, X[[i]])
    resid[,i] <- singleEq[[i]]$resid 
#####    cc <- robreg3S:::.rho.bisquare.tune(NCOL(X[[i]]))
    cc <- .Mpsi.tuning.default("bisquare")
    w <- c(w, wi <- Mwgt(x=resid[,i], cc=cc, psi="bisquare"))
    wY <- c(wY, y*sqrt(wi))
    wX[[i]] <- diag(sqrt(wi))%*%X[[i]]
    p <- c(p, NCOL(X[[i]]))
  }
  X <- bdiag(X)
  wX <- bdiag(wX)

  # residuals covariance
  S <- TSGS(resid)@S
####  S <- kronecker(S, diag(n))
  ## Second step
  eS <- eigenkronecker(S, n)
  d <- eS$values  
  if (any(d <= 0)) 
    stop("'S' is not positive definite")
  v <- t(eS$vector)
  A <- d^(-0.5)*v
  Ainv <- t(d^0.5*v)
  fit <- lm.fit(as.matrix(A %*% wX), A %*% wY, ...)
  beta <- list()
  cp <- c(0,cumsum(p))
  for (i in 2:(m+1)) {
    beta[[i-1]] <- fit$coefficients[(cp[i-1]+1):cp[i]]
  }
  fit$coefficients <- beta
  fit$fitted.values <- matrix(drop(Ainv %*% fit$fitted.values), nrow=n)
  fit$residuals <- matrix(drop(Ainv %*% fit$residuals), nrow=n)
  fit$residCovEst <- TSGS(resid)@S
  fit$residCov <- TSGS(fit$residuals)@S  
  return(fit)
}

if (FALSE) {
library(robustsur)
library(robreg3S)
library(mvtnorm)
m <- 2
p <- 2
n <- 100
rho <- 0.5
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
  formule[[i]] <- y~.
}
res <- surerob2(formule, dati)  

}
