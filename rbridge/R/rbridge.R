#' @useDynLib rbridge, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods is
NULL
#' @importFrom stats model.matrix predict
#' @importFrom graphics abline axis points segments
#' @importClassesFrom Matrix dgCMatrix
#' 
#' @title Fit a Restricted Bridge Estimation
#' @param X Design matrix.
#' @param y Response vector.
#' @param q is the degree of norm which includes ridge regression with \code{q=2} and lasso estimates with \code{q=1} as special cases
#' @param R is \code{m} by \code{p} \code{(m<p)} matrix of constants.
#' @param r is a \code{m}-vector of known prespecified constants. If it is given true restriction, then \deqn{r - R\beta = 0.} Values for \code{r} should be given as a matrix. See "Examples".
#' @param lambda.min The smallest value for lambda if \code{n>p} is \code{0.001} and \code{0.05} otherwise.
#' @param nlambda The number of lambda values - default is \code{100}
#' @param lambda A user supplied lambda sequence. By default, the program compute a squence of values the length of nlambda.
#' @param eta is a preselected small positive threshold value. It is deleted \code{jth} variable to make the algorithm stable and also is excluded \code{jth} variable from the final model. Default is \code{1e-07}.
#' @param converge is the value of converge. Defaults is \code{10^10}. In each iteration, it is calculated by sum of square the change in linear predictor for each coefficient. The algorithm iterates until \code{converge > eta}.

#' @return An object of class rbridge, a list with entries
#' \item{betas}{Coefficients computed over the path of lambda}
#' \item{lambda}{The lambda values which is given at the function}

#' @description
#' Fit a restricted linear model via bridge penalized maximum likelihood.
#' It is computed the regularization path which is consisted of \code{lasso} or \code{ridge} penalty 
#' at the a grid values for \code{lambda}
#' 
#' 
#' @details
#' In order to couple the bridge estimator with the restriction \code{R beta = r}, 
#' we solve the following optimization problem
#'\deqn{\min RSS w.r.t ||\beta||_q and R\beta = r. }
#'
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{cv.rbridge}}
#' 
#' @export
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
#' model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec)
#' print(model1)
#' 
#' ######## Model 2 based on second restrictions
#' model2 <- rbridge(X, y, q = 1, R2.mat, r2.vec)
#' print(model2)
#' 
#' ######## Model 3 based on third restrictions
#' model3 <- rbridge(X, y, q = 1, R3.mat, r3.vec)
#' print(model3)
#' 
#' ######## Model 4 based on fourth restrictions
#' model4 <- rbridge(X, y, q = 1, R4.mat, r4.vec)
#' print(model4)
#' 
rbridge <- function(X, y, q=1, R,r,lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                   lambda, eta=1e-7, converge=10^10) {
  
  # Coersion
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
  
  
  # Error checking
  if (nlambda < 2) stop("nlambda must be at least 2")
  if (q > 2) stop("a must be less than 2; choose a small positive number instead")
  if (any(is.na(y)) | any(is.na(X))) stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to ncvreg")
  if (length(y) != nrow(X)) stop("X and y do not have the same number of observations")
  
  ## Set up XX, yy, lambda
  stdX <- standard(X)
  XX <- stdX$xx
  p <- ncol(XX)
  ns <- c(1:p)
  yy <- y - mean(y)
  n <- length(yy)
  
  if (missing(lambda)) {
    #lambda <- as.double(rev(sort(Lambdas_Grid(XX, yy,q,lambda.min, nlambda))))
    lambda <- Lambdas_Grid(XX, yy,q,lambda.min, nlambda)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
    
  ## Fit
    fit <- RBridge(XX, yy, q, lambda, R, r,converge, eta)
    constant <- rep(mean(y),nlambda)
    betas <- matrix(fit, p, nlambda)
    #loss <- res[[2]]
    #iter <- res[[3]]
    
    
  ## Unstandardize
  unbetas <- matrix(0, nrow=(ncol(X)+1), ncol=length(lambda))
  #bbetas <- betas/attr(XX, "scale")[ns]
  #bbetas <- betas # if unstandard
  bbetas <- betas/as.vector(stdX$s)
  unbetas[ns+1,] <- bbetas  
  unbetas[1,] <- constant - crossprod(as.vector(stdX$c), bbetas)
  
  
  
  ## Names
  varnames <- if (is.null(colnames(X))) paste("V",1:ncol(X),sep="") else colnames(X)
  varnames <- c("(Intercept)", varnames)
  dimnames(unbetas) <- list(varnames, lambda)
  
  
  ## Output
  output <- structure(list(betas = unbetas,
                        lambda = lambda),
                   class = "rbridge")
  output
}
