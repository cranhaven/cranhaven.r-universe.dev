#' Cross-validation for Selecting the Tuning Parameter of Nonconvex Penalized Quantile Regression
#'
#' Conducts k-fold cross-validation for the `nc.hdqr()` function.
#'
#' @param x A numerical matrix with dimensions (\eqn{n} rows and \eqn{p} columns), where each row represents an observation.
#' @param y Response variable.
#' @param lambda Optional user-supplied sequence of \code{lambda} values.
#' @param nfolds Number of folds in the cross-validation, default is 5.
#' @param foldid An optional vector that assigns each observation to a specific fold. 
#'   If provided, this parameter overrides \code{nfolds}.
#' @param tau The quantile level (\code{tau}) used in the error calculation. Default value is typically 0.5 unless specified.
#' @param ... Additional arguments passed to \code{\link{nc.hdqr}}.
#'
#' @details
#' This function estimates the average cross-validation error and its standard error across folds. It is primarily used to 
#' identify the optimal \code{lambda} value for fitting nonconvex penalized quantile regression models.
#'
#' @return
#' An object of class \code{cv.nc.hdqr} is returned,
#' which is a list with the ingredients of the cross-validated fit.
#'  \item{lambda}{the values of \code{lambda} used in the fits.}
#'  \item{cvm}{the mean cross-validated error - a vector of length \code{length(lambda)}.}
#'  \item{cvsd}{estimate of standard error of \code{cvm}.}
#'  \item{cvupper}{upper curve = \code{cvm+cvsd}.}
#'  \item{cvlower}{lower curve = \code{cvm-cvsd}.}
#'  \item{nzero}{number of non-zero coefficients at each \code{lambda}.}
#'  \item{name}{a text string indicating type of measure (for plotting purposes).}
#'  \item{nchdqr.fit}{a fitted \code{\link{nc.hdqr}} object for the full data.}
#'  \item{lambda.min}{The optimal value of \code{lambda} that gives minimum cross validation error \code{cvm}.}
#'  \item{lambda.1se}{The largest value of \code{lambda} such that error is within 1 standard error of the minimum.}
#'
#' @keywords quantile regression
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' beta_star <- c(c(2, 1.5, 0.8, 1, 1.75, 0.75, 0.3), rep(0, (p - 7)))
#' eps <- rnorm(n, mean = 0, sd = 1)
#' y <- x %*% beta_star + eps
#' tau <- 0.5
#' lam2 <- 0.01
#' lambda <- 10^(seq(1,-4, length.out=10))
#' \donttest{cv.nc.fit <- cv.nc.hdqr(y=y, x=x, tau=tau, lambda=lambda, lam2=lam2, pen="scad")}

cv.nc.hdqr <- function(x, y, lambda=NULL, tau, nfolds=5L, foldid, ...) {
  ####################################################################
  ## data setup
  y = drop(y)
  x = as.matrix(x)
  x.row = as.integer(NROW(x))
  if (length(y) != x.row) 
    stop("x and y have different number of observations.")  
  ####################################################################
  nc.hdqr.object = nc.hdqr(x, y, lambda=lambda, tau=tau, ...)
  lambda = nc.hdqr.object$nc.lambda
  nz = sapply(coef(nc.hdqr.object, type="nonzero"), length) 
  if (missing(foldid)) 
    foldid = sample(rep(seq(nfolds), 
      length=x.row)) else nfolds = max(foldid)
  if (nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=5 recommended.")
  outlist = as.list(seq(nfolds))
  ## fit the model nfold times and save them
  for (i in seq(nfolds)) {
    which = foldid == i
    outlist[[i]] = nc.hdqr(x=x[!which, , drop=FALSE], 
      y=y[!which], tau=tau, lambda=lambda, ...)
  }
  ## select the lambda according to predmat
  cvstuff <- cvpath.hdqr(outlist, x, y, tau, lambda, 
    foldid, x.row, ...)

  cvm = cvstuff$cvm
  cvsd = cvstuff$cvsd
  cvname = cvstuff$name
  out = list(lambda=lambda, cvm=cvm, cvsd=cvsd, 
    cvupper=cvm+cvsd, cvlower=cvm - cvsd, nzero=nz,
    name=cvname, nchdqr.fit=nc.hdqr.object)
  obj = c(out, as.list(getmin(lambda, cvm, cvsd)))
  class(obj) = "cv.nc.hdqr"
  obj
} 