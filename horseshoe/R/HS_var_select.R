#' Variable selection using the horseshoe prior
#'
#' The function implements two methods to perform variable selection. The first checks
#' whether 0 is contained in the credible set (see Van der Pas et al. (2016)). The second
#' is only intended for the sparse normal means problem (regression with identity matrix).
#' It is described in Carvalho et al. (2010). The horseshoe posterior mean can be written
#'  as \eqn{c_i y_i}, with \eqn{y_i} the observation. A variable is selected if
#'  \eqn{c_i \geq c}, where \eqn{c} is a user-specified threshold.
#'
#'
#' @param hsobject The outcome from one of the horseshoe functions \code{\link{horseshoe}}
#' or \code{\link{HS.normal.means}}.
#' @param y  The data.
#' @param method Use "intervals" to perform variable selection using the credible sets
#' (at the level specified when creating the hsobject), "threshold" to perform variable
#' selection using the thresholding procedure (only for the sparse normal means problem).
#' @param threshold Threshold for the thresholding procedure. Default is 0.5.
#'
#' @return A vector of zeroes and ones. The ones correspond to the selected variables.
#'
#' @references
#' van der Pas, S.L., Szabo, B., and van der Vaart, A. (2017), Uncertainty
#' quantification for the horseshoe (with discussion). Bayesian Analysis
#' 12(4), 1221-1274.
#'
#' van der Pas, S.L., Szabo, B., and van der Vaart A. (2017), Adaptive
#' posterior contraction rates for the horseshoe. Electronic Journal of
#' Statistics 10(1), 3196-3225.
#'
#'  Carvalho, C. M., Polson, N. G., and Scott, J. G. (2010), The Horseshoe
#'  Estimator for Sparse Signals. Biometrika 97(2), 465–480.
#'
#' @seealso \code{\link{horseshoe}} and \code{\link{HS.normal.means}} to obtain the required
#' hsobject.
#'
#' @examples
#' #Example with 20 signals (last 20 entries), rest is noise
#' truth <- c(rep(0, 80), rep(8, 20))
#' data <-  truth + rnorm(100)
#' horseshoe.results <- HS.normal.means(data, method.tau = "truncatedCauchy",
#'  method.sigma = "fixed")
#' #Using credible sets. Ideally, the first 80 entries are equal to 0,
#' #and the last 20 entries equal to 1.
#' HS.var.select(horseshoe.results, data, method = "intervals")
#' #Using thresholding. Ideally, the first 80 entries are equal to 0,
#' #and the last 20 entries equal to 1.
#' HS.var.select(horseshoe.results, data, method = "threshold")
#'
#' @export
HS.var.select <- function(hsobject, y, method = c("intervals", "threshold"), threshold = 0.5){

  method = match.arg(method)

  if(method == "threshold"){
    proportion <- hsobject$BetaHat / y
    signals <- as.numeric(proportion >= threshold)
    return(signals)
  }

  if(method == "intervals"){
    left <- hsobject$LeftCI
    right <- hsobject$RightCI
    signals <- as.numeric( 1 - ( (left <= 0) & (right >= 0) ))
    return(signals)
  }

}
