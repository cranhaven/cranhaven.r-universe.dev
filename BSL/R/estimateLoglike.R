#' Estimate the synthetic likelihood
#'
#' @description This function computes the estimated synthetic (log) likelihood
#'   using one of the four methods (``BSL'', ``uBSL'', ``semiBSL'' and
#'   ``BSLmisspec''). Please find the links below in the see also section for
#'   more details.
#'
#' @inheritParams gaussianSynLike
#' @inheritParams bsl
#' @param ... Arguments to be passed to methods.
#'
#'   \itemize{
#'
#'   \item \code{shrinkage} Available for methods ``BSL'' and ``semiBSL''. A
#'   string argument indicating which shrinkage method to be used. The default
#'   is \code{NULL}, which means no shrinkage is used. Shrinkage estimation is
#'   only available for methods ``BSL'' and ``semiBSL''. Current options are
#'   ``glasso'' for the graphical lasso method of
#'   \insertCite{Friedman2008;textual}{BSL} and ``Warton'' for the ridge
#'   regularisation method of \insertCite{Warton2008;textual}{BSL}.
#'
#'   \item \code{penalty} Available for methods ``BSL'' and ``semiBSL''. The
#'   penalty value to be used for the specified shrinkage method. Must be
#'   between zero and one if the shrinkage method is ``Warton''.
#'
#'   \item \code{standardise} Available for method ``BSL''. A logical argument
#'   that determines whether to standardise the summary statistics before
#'   applying the graphical lasso. This is only valid if method is ``BSL'',
#'   shrinkage is ``glasso'' and penalty is not \code{NULL}. The diagonal
#'   elements will not be penalised if the shrinkage method is ``glasso''. The
#'   default is \code{FALSE}.
#'
#'   \item \code{GRC} Available for method ``BSL''. A logical argument
#'   indicating whether the Gaussian rank correlation matrix
#'   \insertCite{Boudt2012}{BSL} should be used to estimate the covariance
#'   matrix in ``BSL'' method. The default is \code{FALSE}, which uses the
#'   sample covariance by default.
#'
#'   \item \code{whitening} Available for method ``BSL''. This argument determines
#'   whether Whitening transformation should be used in ``BSL'' method with
#'   Warton's shrinkage. Whitening transformation helps decorrelate the summary
#'   statistics, thus encourages sparsity of the synthetic likelihood covariance
#'   matrix. This might allow heavier shrinkage to be applied without losing
#'   much accuracy, hence allowing the number of simulations to be reduced. By
#'   default, \code{NULL} represents no Whitening transformation. Otherwise this
#'   is enabled if a Whitening matrix is provided. See
#'   \code{\link{estimateWhiteningMatrix}} for the function to estimate the
#'   Whitening matrix.
#'
#'   \item \code{ssyTilde} Available for method ``BSL''. The whitened observed
#'   summary statisic. If this is not \code{NULL}, it will be used to save
#'   computation effort. Only used if Whitening is enabled.
#'
#'   \item \code{kernel} Available for method ``semiBSL''.  A string argument
#'   indicating the smoothing kernel to pass into \code{density} for estimating
#'   the marginal distribution of each summary statistic. Only ``gaussian" and
#'   ``epanechnikov" are available. The default is ``gaussian".
#'
#'   \item \code{type} Available for method ``BSLmisspec''.  A string argument
#'   indicating which method is used to account for and detect potential
#'   incompatibility. The two options are "mean" and "variance".
#'
#'   \item \code{gamma} Available for method ``BSLmisspec''. The additional
#'   latent parameter to account for possible incompatability between the model
#'   and observed summary statistic. In ``BSLmisspec'' method, this is updated
#'   with a slice sampler \insertCite{Neal2003}{BSL}.
#'
#'   }
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
#'                   theta0 = ma2$start)
#' ssx <- simulation(m, n = 300, theta = c(0.6, 0.2), seed = 10)$ssx
#' estimateLoglike(ssy, ssx, method = "BSL")
#' estimateLoglike(ssy, ssx, method = "uBSL")
#' estimateLoglike(ssy, ssx, method = "semiBSL")
#' estimateLoglike(ssy, ssx, method = "BSLmisspec", type = "mean", gamma = rep(0.1, 50))
#'
#' @seealso \code{\link{gaussianSynLike}},
#'   \code{\link{gaussianSynLikeGhuryeOlkin}},
#'   \code{\link{semiparaKernelEstimate}} and \code{\link{synLikeMisspec}}.
#' @export
estimateLoglike <- function(ssy, ssx, method = c("BSL", "uBSL", "semiBSL", "BSLmisspec"), log = TRUE, verbose = FALSE, ...) {
  method <- match.arg(method)
  switch(method,
         "BSL"        = gaussianSynLike(ssy, ssx, log = log, verbose = verbose, ...),
         "uBSL"       = gaussianSynLikeGhuryeOlkin(ssy, ssx, log = log, verbose = verbose, ...),
         "semiBSL"    = semiparaKernelEstimate(ssy, ssx, log = log, ...),
         "BSLmisspec" = synLikeMisspec(ssy, ssx, log = log, verbose = verbose, ...)
  )
}
