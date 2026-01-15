#' @export
#' @title Cross-validation for bridge
#' @param X \code{X} matrix as in bridge.
#' @param y response \code{y} as in bridge.
#' @param q is the degree of norm which includes ridge regression with \code{q=2} and lasso estimates with \code{q=1} as special cases
#' @param lambda lambda sequence; default is NULL. It is given by user or \code{cv.rbridge} chooses its own sequence.
#' @param nfolds number of folds - default is 10.
#' @param lambda.min The smallest value for lambda if \code{n>p} is \code{0.001} and \code{0.05} otherwise.
#' @param nlambda The number of lambda values - default is \code{100}
#' @param eta is a preselected small positive threshold value. It is deleted \code{jth} variable to make the algorithm stable and also is excluded \code{jth} variable from the final model. Default is \code{1e-07}.
#' @param converge is the value of converge. Defaults is \code{10^10}. In each iteration, it is calculated by sum of square the change in linear predictor for each coefficient. The algorithm iterates until \code{converge > eta}.
#' @param num_threads Number of threads used for parallel computation over the folds,

#' @return An object of class rbridge, a list with entries
#' \item{cve}{the mean cross-validated error.}
#' \item{cvse}{estimate of standard error of \code{cvm}.}
#' \item{cvup}{upper curve = \code{cvm+cvsd}.}
#' \item{cvlo}{lower curve = \code{cvm-cvsd}.}
#' \item{lambda}{the values of \code{lambda} used in the fits}
#' \item{nz}{number of non-zero coefficients at each \code{lambda}.}
#' \item{betas}{estimated coefficient at each \code{lambda}.}
#' \item{lambda.min}{value of lambda that gives minimum \code{cve}}
#' \item{lambda.1se}{largest value of \code{lambda} such that error is within 1 standard error of the minimum}



#' @description
#' Does k-fold cross-validation for bridge, produces a plot, and returns a value for lambda
#' 
#' @details
#' Computes bridge
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{bridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 
#' model1 <- cv.bridge(X, y, q = 1)
#' print(model1)
#' coef(model1,s='lambda.min')
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' 
#' ######## Model 2 
#' model2 <- cv.bridge(X, y, q = 2)
#' print(model2)
#' coef(model2,s='lambda.min')
#' predict(model2,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model2, s="lambda.min",type="coefficient")
#' 
cv.bridge = function (X, y, q, lambda,nfolds=10,
                       lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                       eta=1e-7, converge=10^10,num_threads = 10) {
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") 
    storage.mode(X) <- "double"
  
  if (!is(y, "numeric")) {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("y must numeric or able to be coerced to numeric")
  }
  
  


  
  ## Set up XX, yy, lambda
  stdX <- standard(X)
  XX <- stdX$xx
  p <- ncol(XX)
  ns <- c(1:p)
  yy <- y - mean(y)
  n <- length(yy)
  
  
  
  if (missing(lambda)) {
    lambda <- Lambdas_Grid(XX, yy,q,lambda.min, nlambda)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
  
  
  #R <- R1.mat; r <- r1.vec;
  fit <- CV_Bridge(XX,yy,q, lambda, converge=converge, num_folds=nfolds,num_threads=10, eta=eta)
  
  
  
  cve <- apply(fit, 1, sum)
  #cvse <- apply(fit, 1, sd)/sqrt(nlambda)
  cvse <- sqrt(apply(scale(t(fit), cve, FALSE)^2, 2, mean,na.rm = TRUE)/(n - 1))
  #cvse <- sqrt(apply(fit, 1, sd,na.rm = TRUE)/(n - 1))
  
  
  
  bridge.fit = bridge(X, y, q,lambda=lambda, eta=eta)
  
  nz = sapply(predict(bridge.fit, type = "nonzero"),
              length)
  
  val <- list(cve = cve, cvse = cvse, cvup = cve + cvse, 
              cvlo = cve - cvse, lambda = lambda, nz=nz, 
              betas = bridge.fit$betas )
  lamin <- getmin(lambda, cve, cvse) 
  obj = c(val, as.list(lamin))
  class(obj) = "cv.bridge"
  obj
  
}



