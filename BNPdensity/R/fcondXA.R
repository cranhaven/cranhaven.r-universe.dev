#' Conditional density evaluation in the semiparametric model
#'
#' This function evaluates a density path conditionally on a posterior
#' realization of the normalized measure.
#'
#' For internal use.
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(x, distr = 1, Tau, J, sigma) {
#'   pJ <- J / sum(J)
#'   K <- matrix(NA, nrow = length(Tau), ncol = length(x))
#'   for (i in seq(Tau)) {
#'     K[i, ] <- dk(x, distr = distr, mu = Tau[i], sigma = sigma)
#'   }
#'   fcondXA <- apply(K, 2, function(x) sum(x * pJ))
#'   return(fcondXA)
#' }
fcondXA <-
  function(x, distr, Tau, J, sigma) {
    pJ <- J / sum(J)
    K <- matrix(NA, nrow = length(Tau), ncol = length(x))
    for (i in seq(Tau)) {
      K[i, ] <- dk(x, distr = distr, mu = Tau[i], sigma = sigma)
    }
    fcondXA <- apply(K, 2, function(x) sum(x * pJ))
    return(fcondXA)
  }
