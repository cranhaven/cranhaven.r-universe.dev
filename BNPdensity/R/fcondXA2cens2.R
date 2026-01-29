#' Conditional density evaluation in the fully nonparametric model for censored
#' data
#'
#' This function evaluates a density path conditionally on a posterior
#' realization of the normalized measure.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(xleft, xright, censor_code_filters, distr, Tauy, Tauz,
#'          J) {
#'   pJ <- J / sum(J)
#'   K <- matrix(NA, nrow = length(Tauy), ncol = length(xleft))
#'   for (i in seq(Tauy)) {
#'     K[i, ] <- dkcens2(
#'       xleft = xleft, xright = xright, c_code_filters = censor_code_filters,
#'       distr = distr, mu = Tauy[i], sigma = Tauz[i]
#'     )
#'   }
#'   fcondXA2cens <- apply(K, 2, function(x) sum(x * pJ))
#'   return(fcondXA2cens)
#' }
fcondXA2cens2 <-
  function(xleft, xright, censor_code_filters, distr, Tauy, Tauz,
           J) {
    pJ <- J / sum(J)
    K <- matrix(NA, nrow = length(Tauy), ncol = length(xleft))
    for (i in seq(Tauy)) {
      K[i, ] <- dkcens2(
        xleft = xleft, xright = xright, c_code_filters = censor_code_filters,
        distr = distr, mu = Tauy[i], sigma = Tauz[i]
      )
    }
    fcondXA2cens <- apply(K, 2, function(x) sum(x * pJ))
    return(fcondXA2cens)
  }
