#' Estimate the semi-parametric synthetic (log) likelihood
#'
#' @description This function computes the semi-parametric synthetic likelihood
#'   estimator of \insertCite{An2018}{BSL}. The advantage of this
#'   semi-parametric estimator over the standard synthetic likelihood estimator
#'   is that the semi-parametric one is more robust to non-normal summary
#'   statistics. Kernel density estimation is used for modelling each univariate
#'   marginal distribution, and the dependence structure between summaries are
#'   captured using a Gaussian copula. Shrinkage on the correlation matrix
#'   parameter of the Gaussian copula is helpful in decreasing the number of
#'   simulations.
#'
#' @param kernel       A string argument indicating the smoothing kernel to pass
#'   into \code{density} for estimating the marginal distribution of each
#'   summary statistic. Only ``gaussian" and ``epanechnikov" are available. The
#'   default is ``gaussian".
#' @param shrinkage     A string argument indicating which shrinkage method to
#'   be used. The default is \code{NULL}, which means no shrinkage is used.
#'   Current options are ``glasso'' for the graphical lasso method of
#'   \insertCite{Friedman2008;textual}{BSL} and ``Warton'' for the ridge
#'   regularisation method of \insertCite{Warton2008;textual}{BSL}.
#' @inheritParams bsl
#' @inheritParams gaussianSynLike
#'
#' @return             The estimated synthetic (log) likelihood value.
#'
#' @references
#'
#' \insertAllCited{}
#'
#' \insertRef{Friedman2008}{BSL}
#'
#' \insertRef{Warton2008}{BSL}
#'
#' \insertRef{Boudt2012}{BSL}
#'
#' @examples
#' data(ma2)
#' ssy <- ma2_sum(ma2$data)
#' m <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'               theta0 = ma2$start, sumArgs = list(delta = 0.5))
#' ssx <- simulation(m, n = 300, theta = c(0.6, 0.2), seed = 10)$ssx
#'
#' # check the distribution of the first summary statistic: highly non-normal
#' plot(density(ssx[, 1]))
#'
#' # the standard synthetic likelihood estimator over-estimates the likelihood here
#' gaussianSynLike(ssy, ssx)
#' # the semi-parametric synthetic likelihood estimator is more robust to non-normality
#' semiparaKernelEstimate(ssy, ssx)
#' # using shrinkage on the correlation matrix of the Gaussian copula is also possible
#' semiparaKernelEstimate(ssy, ssx, shrinkage = "Warton", penalty = 0.8)
#'
#' @seealso    Other available synthetic likelihood estimators:
#'   \code{\link{gaussianSynLike}} for the standard synthetic likelihood
#'   estimator, \code{\link{gaussianSynLikeGhuryeOlkin}} for the unbiased
#'   synthetic likelihood estimator, \code{\link{synLikeMisspec}} for the
#'   Gaussian synthetic likelihood estimator for model misspecification.
#'
#' @export
semiparaKernelEstimate <- function (ssy, ssx, kernel = "gaussian", shrinkage = NULL,
                                    penalty = NULL, log = TRUE) {
  if (!is.null(shrinkage)) {
    flagShrinkage <- TRUE
    shrinkage <- match.arg(shrinkage, c("glasso", "Warton"))
  } else {
    flagShrinkage <- FALSE
  }
  if (!flagShrinkage && !is.null(penalty)) {
    warning("\"penalty\" will be ignored since no shrinkage method is specified")
  }
  if (flagShrinkage && is.null(penalty)) {
    stop("a penalty value must be specified to enable shrinkage estimation")
  }
  
  n <- nrow(ssx)
  ns <- ncol(ssx)
  stopifnot(ns >= 2)
  stopifnot(length(ssy) == ns)
  
  pdfy <- yu <- numeric(ns)
  for (j in 1 : ns) {
    f <- density(ssx[, j], kernel = kernel, n = 512, from = min(ssx[,j],ssy[j]),
                 to = max(ssx[,j],ssy[j]))
    approxy <- approx(f$x, f$y, ssy[j])
    pdfy[j] <- approxy$y
    yu[j] <- mean(kernelCDF((ssy[j] - ssx[, j]) / f$bw, kernel))
  }
  
  # Gaussian rank correlation
  rhohat <- gaussianRankCorr(ssx, TRUE)
  
  if (!is.null(shrinkage)) {
    RHOHAT <- p2P(rhohat)
    Sigma <- switch(shrinkage,
                    "glasso" = glasso(RHOHAT, rho = penalty, penalize.diagonal = FALSE)$w,
                    "Warton" = corrWarton(RHOHAT, penalty))
    rhohat <- P2p(Sigma)
  }
  
  # density
  copula <- normalCopula(rhohat, dim = ns, dispstr = "un")
  if (log) {
    f <- dCopula(yu, copula, log = TRUE) + sum(log(pdfy))
  } else {
    f <- dCopula(yu, copula, log = FALSE) * prod(pdfy)
  }
  
  return(f)
}
