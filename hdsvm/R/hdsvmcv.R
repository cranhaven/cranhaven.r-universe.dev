#' Cross-validation for Selecting the Tuning Parameter in the Penalized SVM
#'
#' Performs k-fold cross-validation for \code{\link{hdsvm}}, similar to \code{\link[glmnet]{cv.glmnet}}.
#'
#' @param x A numerical matrix with \eqn{n} rows (observations) and \eqn{p} columns (variables).
#' @param y Response variable.
#' @param lambda Optional; a user-supplied sequence of \code{lambda} values. If \code{NULL}, 
#'   \code{\link{hdsvm}} selects its own sequence.
#' @param nfolds Number of folds for cross-validation. Defaults to 5.
#' @param foldid Optional vector specifying the indices of observations in each fold.
#'   If provided, it overrides \code{nfolds}.
#' @param ... Additional arguments passed to \code{\link{hdsvm}}.
#'
#' @details
#' This function computes the average cross-validation error and provides the standard error.
#'
#' @return
#' An object with S3 class \code{cv.hdsvm} consisting of
#'   \item{lambda}{Candidate \code{lambda} values.}
#'   \item{cvm}{Mean cross-validation error.}
#'   \item{cvsd}{Standard error of the mean cross-validation error.}
#'   \item{cvup}{Upper confidence curve: \code{cvm} + \code{cvsd}.}
#'   \item{cvlo}{Lower confidence curve: \code{cvm} - \code{cvsd}.}
#'   \item{lambda.min}{\code{lambda} achieving the minimum cross-validation error.}
#'   \item{lambda.1se}{Largest \code{lambda} within one standard error of the minimum error.}
#'   \item{cv.min}{Cross-validation error at \code{lambda.min}.}
#'   \item{cv.1se}{Cross-validation error at \code{lambda.1se}.}
#'   \item{hdsvm.fit}{a fitted \code{\link{hdsvm}} object for the full data.}
#'   \item{nzero}{Number of non-zero coefficients at each \code{lambda}.}
#' @keywords models classification
#' @export
#' @examples
#' set.seed(315)
#' n <- 100
#' p <- 400
#' x1 <- matrix(rnorm(n / 2 * p, -0.25, 0.1), n / 2)
#' x2 <- matrix(rnorm(n / 2 * p, 0.25, 0.1), n / 2)
#' x <- rbind(x1, x2)
#' beta <- 0.1 * rnorm(p)
#' prob <- plogis(c(x %*% beta))
#' y <- 2 * rbinom(n, 1, prob) - 1
#' lam2 <- 0.01
#' fit <- cv.hdsvm(x, y, lam2=lam2)

cv.hdsvm <- function(x, y, lambda=NULL, nfolds=5L, foldid, ...) {
  ####################################################################
  ## data setup
  y <- drop(y)
  x <- as.matrix(x)
  x.row <- as.integer(NROW(x))
  if (length(y) != x.row) 
    stop("x and y have different number of observations.")  
  ####################################################################
  if (is.null(lambda)) {
    hdsvm.object <- hdsvm(x, y, lambda=lambda, ...)
    lambda <- hdsvm.object$lambda
  } else {
    hdsvm.object <- hdsvm(x, y, lambda=lambda, ...)
  }
  nz <- sapply(coef(hdsvm.object, type="nonzero"), length) 
  if (missing(foldid)) 
    foldid <- sample(rep(seq(nfolds), 
      length=x.row)) else nfolds = max(foldid)
  if (nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=5 recommended.")
  outlist <- as.list(seq(nfolds))
  ## fit the model nfold times and save them
  for (i in seq(nfolds)) {
    which <- foldid == i
    outlist[[i]] <- hdsvm(x=x[!which, , drop=FALSE], 
      y=y[!which], lambda=lambda, ...)
  }
  ## select the lambda according to predmat
  cvstuff <- cvpath.hdsvm(outlist, x, y, lambda, 
    foldid, x.row, ...)

  cvm <- cvstuff$cvm
  cvsd <- cvstuff$cvsd
  cvname <- cvstuff$name
  out <- list(lambda=lambda, cvm=cvm, cvsd=cvsd, 
    cvupper=cvm+cvsd, cvlower=cvm - cvsd, nzero=nz,
    name=cvname, hdsvm.fit=hdsvm.object)
  obj <- c(out, as.list(getmin(lambda, cvm, cvsd)))
  class(obj) <- "cv.hdsvm"
  obj
} 

cvpath.hdsvm <- function(outlist, x, y, lambda, foldid, x.row, ...) {
  nfolds <- max(foldid)
  predmat <- matrix(NA, x.row, length(lambda))
  nlams <- double(nfolds)
  for (i in seq(nfolds)) {
    whichfold <- foldid == i
    fitobj <- outlist[[i]]
    preds <- predict(fitobj, x[whichfold, , drop = FALSE], type = "loss")
    nlami <- length(fitobj$lambda)
    predmat[whichfold, seq(nlami)] <- preds
    nlams[i] <- nlami
  }
  cvraw <- svm_loss(y * predmat)
  N <- length(y) - apply(is.na(predmat), 2, sum)
  cvm <- colMeans(cvraw, na.rm = TRUE)
  scaled <- scale(cvraw, cvm, FALSE)^2
  cvsd <- sqrt(colMeans(scaled, na.rm = TRUE) / (N - 1))
  out <- list(cvm=cvm, cvsd=cvsd, cvraw=cvraw)
  out
}

svm_loss <- function(tval) pmax(1 - tval, 0)