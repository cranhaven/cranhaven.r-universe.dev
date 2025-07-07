#' Compute CV statistics
#'
#' Use the returned output from `computeRawError()` to compute CV statistics.
#'
#' @param cvstuff Output from a call to `computeRawError()`.
#' @param foldid Vector of values identifying which fold each observation is
#' in.
#' @param lambda Lambda values associated with the errors in `cvstuff`.
#' @param grouped Experimental argument; see `kfoldcv()` documentation for
#' details.
#'
#' @return A list with the following elements:
#' \item{lambda}{The values of lambda used in the fits.}
#' \item{cvm}{The mean cross-validated error: a vector of length
#' `length(lambda)`.}
#' \item{cvsd}{Estimate of standard error of `cvm`.}
#' \item{cvup}{Upper curve = `cvm + cvsd`.}
#' \item{cvlo}{Lower curve = `cvm - cvsd`.}
#'
#' @importFrom stats weighted.mean
computeStats <- function(cvstuff, foldid, lambda, grouped) {
  if (grouped) {
    # compute the statistics for each fold
    nlams <- dim(cvstuff$cvraw)[2]
    cvstuff <- cvcompute(cvstuff, foldid, nlams)
  }

  cvm <- with(cvstuff, apply(cvraw, 2, weighted.mean, w = weights, na.rm = TRUE))
  cvsd <- with(cvstuff, sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean,
                                w = weights, na.rm = TRUE)/(N - 1)))
  nas <- is.na(cvsd)
  if (any(nas)) {
    lambda <- lambda[!nas]
    cvm <- cvm[!nas]
    cvsd <- cvsd[!nas]
  }
  return(list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + cvsd,
              cvlo = cvm - cvsd))
}

#' @importFrom stats weighted.mean
cvcompute <- function (cvstuff, foldid, nlams) {
  foldid_vals <- sort(unique(foldid))
  nfolds <- length(foldid_vals)

  weights <- cvstuff$weights
  mat <- cvstuff$cvraw
  wisum <- tapply(weights, foldid, sum)
  outmat <- matrix(NA, nfolds, ncol(mat))
  good <- matrix(0, nfolds, ncol(mat))
  mat[is.infinite(mat)] <- NA
  for (i in seq_along(foldid_vals)) {
    mati <- mat[foldid == foldid_vals[i], , drop = FALSE]
    wi <- weights[foldid == foldid_vals[i]]
    outmat[i, ] <- apply(mati, 2, weighted.mean, w = wi, na.rm = TRUE)
    good[i, seq(nlams)] <- 1
  }
  N <- apply(good, 2, sum)
  return(list(cvraw = outmat, weights = wisum, N = N,
              type.measure = cvstuff$type.measure))
}
