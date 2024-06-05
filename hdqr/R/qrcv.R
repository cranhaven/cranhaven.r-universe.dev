#' Cross-validation for Selecting the Tuning Parameter in Elastic Net Penalized
#' Quantile Regression
#'
#' Performs k-fold cross-validation for \code{\link{hdqr}}, similar to \code{\link[glmnet]{cv.glmnet}}.
#'
#' @param x A numerical matrix with \eqn{n} rows (observations) and \eqn{p} columns (variables).
#' @param y Response variable.
#' @param lambda Optional; a user-supplied sequence of \code{lambda} values. If \code{NULL}, 
#'   \code{\link{hdqr}} selects its own sequence.
#' @param nfolds Number of folds for cross-validation. Defaults to 5.
#' @param foldid Optional vector specifying the indices of observations in each fold.
#'   If provided, it overrides \code{nfolds}.
#' @param tau Quantile level (\code{tau}) used in the loss function.
#' @param ... Additional arguments passed to \code{\link{hdqr}}.
#'
#' @details
#' This function computes the average cross-validation error and provides the standard error.
#'
#' @return
#' Returns an object of class \code{cv.hdqr}, which is a list with components that describe 
#' the cross-validation results:
#' \describe{
#'   \item{lambda}{Candidate \code{lambda} values.}
#'   \item{cvm}{Mean cross-validation error.}
#'   \item{cvsd}{Standard error of the mean cross-validation error.}
#'   \item{cvup}{Upper confidence curve: \code{cvm} + \code{cvsd}.}
#'   \item{cvlo}{Lower confidence curve: \code{cvm} - \code{cvsd}.}
#'   \item{lambda.min}{\code{lambda} achieving the minimum cross-validation error.}
#'   \item{lambda.1se}{Largest \code{lambda} within one standard error of the minimum error.}
#'   \item{cv.min}{Cross-validation error at \code{lambda.min}.}
#'   \item{cv.1se}{Cross-validation error at \code{lambda.1se}.}
#'   \item{hdqr.fit}{a fitted \code{\link{hdqr}} object for the full data.}
#'   \item{nzero}{Number of non-zero coefficients at each \code{lambda}.}
#'}
#' @keywords models regression
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x <- matrix(data = rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
#' beta_star <- c(c(2, 1.5, 0.8, 1, 1.75, 0.75, 0.3), rep(0, (p - 7)))
#' eps <- rnorm(n, mean = 0, sd = 1)
#' y <- x %*% beta_star + eps
#' tau <- 0.5
#' cv.fit <- cv.hdqr(x = x, y = y, tau = tau)
#' @export
#'


cv.hdqr <- function(x, y, lambda=NULL, tau, nfolds=5L, foldid, ...) {
  ####################################################################
  ## data setup
  y <- drop(y)
  x <- as.matrix(x)
  x.row <- as.integer(NROW(x))
  if (length(y) != x.row) 
    stop("x and y have different number of observations.")  
  ####################################################################
  hdqr.object <- hdqr(x, y, lambda=lambda, tau=tau, ...)
  lambda <- hdqr.object$lambda
  nz <- sapply(coef(hdqr.object, type="nonzero"), length) 
  if (missing(foldid)) 
    foldid <- sample(rep(seq(nfolds), 
      length=x.row)) else nfolds = max(foldid)
  if (nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=5 recommended.")
  outlist <- as.list(seq(nfolds))
  ## fit the model nfold times and save them
  for (i in seq(nfolds)) {
    which <- foldid == i
    outlist[[i]] <- hdqr(x=x[!which, , drop=FALSE], 
      y=y[!which], tau=tau, lambda=lambda, ...)
  }
  ## select the lambda according to predmat
  cvstuff <- cvpath.hdqr(outlist, x, y, tau, lambda, 
    foldid, x.row, ...)

  cvm <- cvstuff$cvm
  cvsd <- cvstuff$cvsd
  cvname <- cvstuff$name
  out <- list(lambda = lambda, cvm = cvm, cvsd = cvsd,
              cvupper = cvm + cvsd, cvlower = cvm - cvsd,
              nzero = nz, name = cvname,
              hdqr.fit = hdqr.object)
  obj = c(out, as.list(getmin(lambda, cvm, cvsd)))
  class(obj) = "cv.hdqr"
  obj
} 

cvpath.hdqr <- function(outlist, x, y, tau, lambda, foldid, x.row, ...) {
  nfolds <- max(foldid)
  predmat <- matrix(NA, x.row, length(lambda))
  nlams <- double(nfolds)
  for (i in seq(nfolds)) {
    whichfold <- foldid == i
    fitobj <- outlist[[i]]
    preds <- predict(fitobj, x[whichfold, , drop = FALSE])
    nlami <- length(fitobj$lambda)
    predmat[whichfold, seq(nlami)] <- preds
    nlams[i] <- nlami
  }
  cvraw <- check_loss(y-predmat, tau)
  N <- length(y) - apply(is.na(predmat), 2, sum)
  cvm <- colMeans(cvraw, na.rm = TRUE)
  scaled <- scale(cvraw, cvm, FALSE)^2
  cvsd <- sqrt(colMeans(scaled, na.rm = TRUE) / (N - 1))
  out <- list(cvm=cvm, cvsd=cvsd, cvraw=cvraw)
  out
} 