#' Estimate the Whitening matrix to be used in the ``wBSL'' method of
#' \insertCite{Priddle2019;textual}{BSL}
#'
#' @description This function estimates the Whitening matrix to be used in BSL
#'   with Warton's shrinkage and Whitening (``wBSL'' method of
#'   \insertCite{Priddle2019;textual}{BSL}). The Whitening transformation and
#'   decorrelation methods are detailed in \insertCite{Kessy2018;textual}{BSL}.
#'
#' @param n The number of model simulations to estimate the Whitening matrix.
#' @param method The type of Whitening method to be used. The default is
#'   ``PCA''.
#' @param thetaPoint A point estimate of the parameter value with non-negligible
#'   posterior support.
#' @inheritParams bsl
#' @return The estimated Whitening matrix.
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' \dontshow{
#' data(ma2)
#' model <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args, theta0 = ma2$start)
#' W <- estimateWhiteningMatrix(500, model, method = "PCA", thetaPoint = c(0.6, 0.2))
#' }
#' \dontrun{
#' data(ma2)
#' model <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args, theta0 = ma2$start)
#' W <- estimateWhiteningMatrix(20000, model, method = "PCA", thetaPoint = c(0.6, 0.2))
#' }
#'
#' @export
estimateWhiteningMatrix <- function(n, model, method = c("PCA", "ZCA", "Cholesky", "ZCA-cor", "PCA-cor"),
                                    thetaPoint = NULL, parallel = FALSE, parallelArgs = NULL) {
  method <- match.arg(method)
  if (parallel) {
    myFnSimSum <- function(n, theta) fn(model)$fnPar(n, theta, parallelArgs)
  } else {
    myFnSimSum <- fn(model)$fn
  }
  if (is.null(thetaPoint)) {
    thetaPoint <- model@theta0
  }
  ssx <- myFnSimSum(n, thetaPoint)
  S <- cov(ssx)
  W <- whitening::whiteningMatrix(S, method = method)
  return(W)
}
