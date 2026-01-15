#' @export
#' @title Fit a Bridge Estimation
#' @param X Design matrix.
#' @param y Response vector.
#' @param q is the degree of norm which includes ridge regression with \code{q=2} and lasso estimates with \code{q=1} as special cases
#' @param lambda.min The smallest value for lambda if \code{n>p} is \code{0.001} and \code{0.05} otherwise.
#' @param nlambda The number of lambda values - default is \code{100}
#' @param lambda A user supplied lambda sequence. By default, the program compute a squence of values the length of nlambda.
#' @param eta is a preselected small positive threshold value. It is deleted \code{jth} variable to make the algorithm stable and also is excluded \code{jth} variable from the final model. Default is \code{1e-07}.
#' @param converge is the value of converge. Defaults is \code{10^10}. In each iteration, it is calculated by sum of square the change in linear predictor for each coefficient. The algorithm iterates until \code{converge > eta}.

#' @return An object of class rbridge, a list with entries
#' \item{betas}{Coefficients computed over the path of lambda}
#' \item{lambda}{The lambda values which is given at the function}

#' @description
#' Fit a bridge penalized maximum likelihood.
#' It is computed the regularization path which is consisted of \code{lasso} or \code{ridge} penalty 
#' at the a grid values for \code{lambda}
#' 
#' @details
#' Computes bridge estimation
#' 
#' 
#' @author Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' 
#' @seealso \code{\link{cv.bridge}}
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
#' model1 <- bridge(X, y, q = 1)
#' print(model1)
#' 
#' model2 <- bridge(X, y, q = 2)
#' print(model2)
#' 
bridge <- function(X, y, q=1, lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                    lambda, eta=1e-7, converge=10^10) {
  
  # Coersion
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") 
    storage.mode(X) <- "double"
  
  if (!is(y, "numeric")) {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("y must numeric or able to be coerced to numeric")
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
  fit <- Bridge(XX, yy, q, lambda, converge,eta)
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
                      class = "bridge")
  output
}
