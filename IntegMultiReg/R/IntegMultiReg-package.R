#' IntegMultiReg: Integrative Bayesian Multiple Regression for Multi-Platform Biomarkers
#'
#' The \pkg{IntegMultiReg} package implements the integrative multi-regression
#' (IMR) approach of Chekouo et al. (2017) and extends it from time-to-event
#' outcomes to continuous (Gaussian) and binary (probit) outcomes.  Subjects
#' are partitioned into availability subgroups according to which platforms
#' they have measured, one regression model is built per subgroup, and
#' information is shared across availability subgroups through a Markov random
#' field prior on the
#' variable-selection indicators combined with non-local priors on the
#' regression coefficients.
#'
#' The main entry points are:
#' \describe{
#'   \item{\code{\link{imr}}}{Fit the model by MCMC and return an object of
#'     class \code{"imr"}.}
#'   \item{\code{\link{predict.imr}}}{Predict outcomes for new subjects.}
#'   \item{\code{\link{cv_imr}}}{Assess predictive performance by
#'     cross-validation.}
#' }
#' Standard methods \code{\link{print.imr}}, \code{\link{summary.imr}},
#' \code{\link{coef.imr}}, \code{\link{plot.imr}} and \code{\link{predict.imr}}
#' are provided for the fitted object, together with the stand-alone displays
#' \code{\link{plot_top_features}} and \code{\link{plot_subgroup_sizes}}.
#'
#' Jianfeng Wang initiated the R interface that calls the C implementation.
#'
#' @references
#' Chekouo T, Stingo FC, Doecke JD, Do K-A (2017). "A Bayesian Integrative
#' Approach for Multi-Platform Genomic Data: A Kidney Cancer Case Study."
#' \emph{Biometrics}, \strong{73}(2), 615--624. \doi{10.1111/biom.12587}
#'
#' @keywords internal
#' @useDynLib IntegMultiReg, .registration = TRUE
#' @importFrom stats sd pnorm
#' @importFrom graphics abline axis box image par
#' @importFrom grDevices colorRampPalette
#' @importFrom utils capture.output
"_PACKAGE"
