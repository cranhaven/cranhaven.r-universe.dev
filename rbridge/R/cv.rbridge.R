#' @export
#' @title Cross-validation for rbridge
#' @param X \code{X} matrix as in rbridge.
#' @param y response \code{y} as in rbridge.
#' @param q is the degree of norm which includes ridge regression with \code{q=2} and lasso estimates with \code{q=1} as special cases
#' @param R is \code{m} by \code{p} \code{(m<p)} matrix of constants.
#' @param r is a \code{m}-vector of known prespecified constants. If it is given true restriction, then \deqn{r - R\beta = 0.} Values for \code{r} should be given as a matrix. See "Examples".
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
#' Does k-fold cross-validation for rbridge, produces a plot, and returns a value for lambda
#' 
#' @details
#' Computes cv.rbridge
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{rbridge}}
#' 
#' 
#' @examples
#' set.seed(2019) 
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' p.active <- which(beta != 0)
#' 
#' ### Restricted Matrix and vector
#' ### Res 1
#' c1 <- c(1,1,0,0,1,0,0,0)
#' R1.mat <- matrix(c1,nrow = 1, ncol = p)
#' r1.vec <- as.matrix(c(6.5),1,1)
#' ### Res 2
#' c2 <- c(-1,1,0,0,1,0,0,0)
#' R2.mat <- matrix(c2,nrow = 1, ncol = p)
#' r2.vec <- matrix(c(0.5),nrow = 1, ncol = 1)
#' ### Res 3
#' R3.mat <- t(matrix(c(c1,c2),nrow = p, ncol = 2))
#' r3.vec <- matrix(c(6.5,0.5),nrow = 2, ncol = 1)
#' ### Res 4
#' R4.mat = diag(1,p,p)[-p.active,]
#' r4.vec <- matrix(rep(0,p-length(p.active)),nrow = p-length(p.active), ncol = 1)
#' 
#' n = 100
#' X = matrix(rnorm(n*p),n,p)
#' y = X%*%beta + rnorm(n) 
#' 
#' ######## Model 1 based on first restrictions
#' model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec)
#' print(model1)
#' coef(model1,s='lambda.min')
#' coef(model1,s='lambda.1se')
#' predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model1, s="lambda.min",type="coefficient")
#' predict(model1, s="lambda.1se",type="coefficient")
#' 
#' ######## Model 2 based on second restrictions
#' model2 <- cv.rbridge(X, y, q = 1, R2.mat, r2.vec)
#' print(model2)
#' coef(model2,s='lambda.min')
#' coef(model2,s='lambda.1se')
#' predict(model2,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model2, s="lambda.min",type="coefficient")
#' predict(model2, s="lambda.1se",type="coefficient")
#' 
#' ######## Model 3 based on third restrictions
#' model3 <- cv.rbridge(X, y, q = 1, R3.mat, r3.vec)
#' print(model3)
#' coef(model3,s='lambda.min')
#' coef(model3,s='lambda.1se')
#' predict(model3,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model3, s="lambda.min",type="coefficient")
#' predict(model3, s="lambda.1se",type="coefficient")
#' 
#' ######## Model 4 based on fourth restrictions
#' model4 <- cv.rbridge(X, y, q = 1, R4.mat, r4.vec)
#' print(model4)
#' coef(model4,s='lambda.min')
#' coef(model4,s='lambda.1se')
#' predict(model4,newx=X[1:5,], s="lambda.min", type="response")
#' predict(model4, s="lambda.min",type="coefficient")
#' predict(model4, s="lambda.1se",type="coefficient")
#' 
#' 
cv.rbridge = function (X, y, q, R,r, lambda,nfolds=10,
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
  fit <- CV_RBridge(XX,yy,q, lambda, R,r, eta=eta, converge=converge, num_folds=nfolds ,num_threads=10)
  cve <- apply(fit, 1, sum)
  cvse <- sqrt(apply(scale(t(fit), cve, FALSE)^2, 2, mean,na.rm = TRUE)/(n - 1))
  rbridge.fit = rbridge(X, y, q, R,r,lambda=lambda,eta=eta)
  nz = sapply(predict(rbridge.fit, type = "nonzero"),
              length)
  val <- list(cve = cve, cvse = cvse, cvup = cve + cvse, 
              cvlo = cve - cvse, lambda = lambda, nz=nz, 
              betas = rbridge.fit$betas )
  lamin <- getmin(lambda, cve, cvse) 
  obj = c(val, as.list(lamin))
  class(obj) = "cv.rbridge"
  obj
}




