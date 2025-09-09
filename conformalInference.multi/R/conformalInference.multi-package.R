#'Conformal Inference Tools for Regression with Multivariate Response
#'
#'@description It computes full conformal, split conformal and multi split conformal prediction
#'regions when the response variable is multivariate (i.e. dimension is greater than one).
#'Moreover, the package also contain plot functions to visualize the output of the full and
#'split conformal functions.
#'
#' @details Conformal inference is a framework for converting any pre-chosen
#' estimator of
#'   the regression function into prediction regions with finite-sample
#'   validity, under essentially no assumptions on the data-generating process
#'   (aside from the the assumption of i.i.d. observations). The main functions
#'   in this package for computing such prediction regions are
#'   \code{\link{conformal.multidim.split}} , i.e. a single split, and
#'    \code{\link{conformal.multidim.msplit}} , i.e. joining B splits.
#'   To guarantee consistency, the package structure mimics the univariate
#' 'conformalInference' package of professor Ryan Tibshirani.
#'
#' @references
#' \itemize{
#'  \item{"Distribution-Free Predictive Inference For Regression" by Lei et al. (2016) <arXiv:1604.04173>}
#'  \item{"Conformal Prediction Bands
#' for Multivariate Functional Data" by Diquigiovanni, Fontana, and Vantini (2021)
#' <arXiv:2106.01792>}
#'  \item{"The Importance of Being a Band: Finite-Sample Exact Distribution-Free
#' Prediction Sets for Functional Data" by Diquigiovanni, Fontana, and Vantini (2021) <arXiv:2102.06746>}
#'  \item{"Multi Split Conformal Prediction" by Solari, and Djordjilovic (2021) <arXiv:2103.00627>}
#' }
#'
#' @keywords internal
"_PACKAGE"
