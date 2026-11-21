#' Fitter Function for Ordered Probit Switching Regression Models
#'
#' This is the basic computing engine called by [`opsr`] used to fit ordinal
#' probit switching regression models. Should usually *not* be used directly.
#' The log-likelihood function is implemented in C++ which yields a considerable
#' speed-up. Parallel computation is implemented using `OpenMP`.
#'
#' @param Ws list of matrices with explanatory variables for selection process for each regime.
#' @param Xs list of matrices with expalanatory varialbes for outcome process for each regime.
#' @param Ys list of vectors with continuous outcomes for each regime.
#' @param start a numeric vector with the starting values (passed to [`maxLik::maxLik`]).
#' @param fixed parameters to be treated as constants at their `start` values. If
#'   present, it is treated as an index vector of `start` parameters (passed to [`maxLik::maxLik`]).
#' @param weights a vector of weights to be used in the fitting process. Has to
#'   conform with order (`w <- weights[order(Z)]`, where Z is the ordinal
#'   outcome).
#' @param method maximzation method (passed to [`maxLik::maxLik`]).
#' @param iterlim maximum number of iterations (passed to [`maxLik::maxLik`]).
#' @param printLevel larger number prints more working information (passed to [`maxLik::maxLik`]).
#' @param nThreads number of threads to be used. Do not pass higher number than
#'   number of ordinal outcomes. See also [`opsr_check_omp`] and [`opsr_max_threads`].
#' @param .useR if `TRUE`, usese [`loglik_R`]. Go grab a coffe.
#' @param .loglik if `TRUE`, returns the vector of log-likelihood values given
#'   the parameters passed via `start`.
#' @param ... further arguments passed to [`maxLik::maxLik`].
#'
#' @return object of class `"maxLik" "maxim"`.
#'
#' @seealso [`maxLik::maxLik`], [`loglik_cpp`], [`opsr`]
#' @export
opsr.fit <- function(Ws, Xs, Ys, start, fixed, weights,
                     method, iterlim, printLevel, nThreads, .useR = FALSE,
                     .loglik = FALSE, ...) {
  nReg <- length(Xs)

  ll <- ifelse(.useR, loglik_R, loglik_cpp)

  ll2 <- function(theta) ll(theta, Ws, Xs, Ys, weights, nReg, nThreads)

  if (.loglik) return(ll2(start))

  mL <- maxLik::maxLik(ll2, start = start, fixed = fixed, method = method, iterlim = iterlim,
                       printLevel = printLevel, ...)

  mL
}
