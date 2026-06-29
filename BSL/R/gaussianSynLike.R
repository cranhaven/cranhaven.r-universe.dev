#' Estimate the Gaussian synthetic (log) likelihood
#'
#' @description This function estimates the Gaussian synthetic log-likelihood
#'   \insertCite{@see @Wood2010 and @Price2018}{BSL}. Several extensions are
#'   provided in this function: \code{shrinkage} enables shrinkage estimation of
#'   the covariance matrix and is helpful to bring down the number of model
#'   simulations (see \insertCite{An2019;textual}{BSL} for an example of BSL
#'   with glasso \insertCite{Friedman2008}{BSL} shrinkage estimation);
#'   \code{GRC} uses Gaussian rank correlation \insertCite{Boudt2012}{BSL} to
#'   find a more robust correlation matrix; \code{whitening}
#'   \insertCite{Kessy2018}{BSL} could further reduce the number of model
#'   simulations upon Warton's shrinkage \insertCite{Warton2008}{BSL} by
#'   decorrelating the summary statistics.
#'
#' @param ssy          The observed summary statisic.
#' @param ssx          A matrix of the simulated summary statistics. The number
#'   of rows is the same as the number of simulations per iteration.
#' @param ssyTilde     The whitened observed summary statisic. If this is not
#'   \code{NULL}, it will be used to save computation effort. Only used if
#'   Whitening is enabled.
#' @param log          A logical argument indicating if the log of likelihood is
#'   given as the result. The default is \code{TRUE}.
#' @param verbose      A logical argument indicating whether an error message
#'   should be printed if the function fails to compute a likelihood. The
#'   default is \code{FALSE}.
#' @inheritParams bsl
#'
#' @return             The estimated synthetic (log) likelihood value.
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' data(ma2)
#' ssy <- ma2_sum(ma2$data)
#' m <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'               theta0 = ma2$start)
#' ssx <- simulation(m, n = 300, theta = c(0.6, 0.2), seed = 10)$ssx
#'
#' # the standard Gaussian synthetic likelihood (the likelihood estimator used in BSL)
#' gaussianSynLike(ssy, ssx)
#' # the Gaussian synthetic likelihood with glasso shrinkage estimation
#' # (the likelihood estimator used in BSLasso)
#' gaussianSynLike(ssy, ssx, shrinkage = 'glasso', penalty = 0.1)
#' # the Gaussian synthetic likelihood with Warton's shrinkage estimation
#' gaussianSynLike(ssy, ssx, shrinkage = 'Warton', penalty = 0.9)
#' # the Gaussian synthetic likelihood with Warton's shrinkage estimation and Whitening transformation
#' W <- estimateWhiteningMatrix(20000, m)
#' gaussianSynLike(ssy, ssx, shrinkage = 'Warton', penalty = 0.9, whitening = W)
#'
#' @seealso    Other available synthetic likelihood estimators:
#'   \code{\link{gaussianSynLikeGhuryeOlkin}} for the unbiased synthetic
#'   likelihood estimator, \code{\link{semiparaKernelEstimate}} for the
#'   semi-parametric likelihood estimator, \code{\link{synLikeMisspec}} for the
#'   Gaussian synthetic likelihood estimator for model misspecification.
#'
#' @export
gaussianSynLike <- function(ssy, ssx, shrinkage = NULL, penalty = NULL, standardise = FALSE, GRC = FALSE,
                            whitening = NULL, ssyTilde = NULL, log = TRUE, verbose = FALSE) {
  if (!is.null(shrinkage)) {
    flagShrinkage <- TRUE
    shrinkage <- match.arg(shrinkage, c("glasso", "Warton"))
  } else {
    flagShrinkage <- FALSE
  }
  if (is.null(whitening)) {
    flagWhitening <- FALSE
    ssyTilde <- NULL
  } else if (is.atomic(whitening) & is.matrix(whitening)) {
    ns <- length(ssy)
    if (all(dim(whitening) == c(ns, ns))) {
      flagWhitening <- TRUE
    } else {
      stop(paste("The Whitening matrix must be of dimension", ns, "by", ns))
    }
  } else {
    stop("invalid argument \"whitening\"")
  }
  if (!flagShrinkage && !is.null(penalty)) {
    warning('"penalty" will be ignored because no shrinkage method is specified')
  }
  if (flagShrinkage && is.null(penalty)) {
    stop('a penalty value must be specified to enable shrinkage estimation')
  }
  if (!flagShrinkage && standardise) {
    warning('"standardise" will be ignored because shrinkage method is not "glasso"')
  }
  if (!flagShrinkage && flagWhitening) {
    warning('"whitening" will be ignored because shrinkage method is not "Warton"')
  }
  if (flagShrinkage) {
    if (shrinkage != 'glasso' && standardise) {
      warning("standardisation is only supported if shrinkage is \"glasso\"")
    }
    if (shrinkage != 'Warton' && flagWhitening) {
      warning("Whitening is only supported if shrinkage is \"Warton\"")
    }
  }
  
  
  if (!flagShrinkage) { # BSL if no shrinkage
    mu <- colMeans(ssx)
    if (GRC) {
      std <- apply(ssx, MARGIN = 2, FUN = sd)
      corr <- gaussianRankCorr(ssx)
      Sigma <- cor2cov(corr, std)
    } else {
      Sigma <- cov(ssx)
    }
  } else { # BSL with shrinkage (glasso or Warton)
    mu <- colMeans(ssx)
    
    if (shrinkage == 'glasso') {
      if (!standardise) { # use graphical lasso without standardisation
        if (GRC) {
          std <- apply(ssx, MARGIN = 2, FUN = sd)
          corr <- gaussianRankCorr(ssx)
          S <- cor2cov(corr, std)
        } else {
          S <- cov(ssx)
        }
        gl <- glasso(S, rho = penalty)
        Sigma <- gl$w
      } else { # standardise the summary statistics before passing into the graphical lasso function
        n <- nrow(ssx)
        ns <- ncol(ssx)
        std <- apply(ssx, MARGIN = 2, FUN = sd)
        ssx_std <- (ssx - matrix(mu, n, ns, byrow = TRUE)) / matrix(std, n, ns, byrow = TRUE)
        if (GRC) {
          corr <- gaussianRankCorr(ssx_std)
          S <- cor2cov(corr, apply(ssx_std, MARGIN = 2, FUN = sd))
        } else {
          S <- cov(ssx_std)
        }
        gl <- glasso(S, rho = penalty, penalize.diagonal = FALSE) # do not penalise the diagonal entries since we want the correlation matrix
        corr <- gl$w
        Sigma <- outer(std, std) * corr
      }
    }
    
    if (shrinkage == 'Warton') {
      if (flagWhitening) { # Whitening transformation
        if (is.null(ssyTilde)) {
          ssy <- c(tcrossprod(ssy, whitening))
        } else { # inherit ssy to save computation
          ssy <- ssyTilde
        }
        ssx <- tcrossprod(ssx, whitening)
        mu <- c(tcrossprod(mu, whitening))
      }
      if (GRC) {
        std <- apply(ssx, MARGIN = 2, FUN = sd)
        corr <- gaussianRankCorr(ssx)
        S <- cor2cov(corr, std)
      } else {
        S <- cov(ssx)
      }
      Sigma <- covWarton(S, penalty)
    }
  }
  
  loglike <- try(mvtnorm::dmvnorm(ssy, mean = mu, sigma = Sigma, log = log))
  if (inherits(loglike, 'try-error')) {
    if (verbose) {
      cat('*** reject (probably singular covariance matrix) ***\n')
    }
    return (-Inf)
  }
  return (loglike)
}
