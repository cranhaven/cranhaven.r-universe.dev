#' Conditional density evaluation in the fully nonparametric model
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
#' function(x, distr = 1, Tauy, Tauz, J) {
#'   pJ <- J / sum(J)
#'   K <- matrix(NA, nrow = length(Tauy), ncol = length(x))
#'   for (i in seq(Tauy)) {
#'     K[i, ] <- dk(x, distr = distr, mu = Tauy[i], sigma = Tauz[i])
#'   }
#'   fcondXA2 <- apply(K, 2, function(x) sum(x * pJ))
#'   return(fcondXA2)
#' }
fcondXA2 <-
  function(x, distr, Tauy, Tauz, J) {
    pJ <- J / sum(J)
    K <- matrix(NA, nrow = length(Tauy), ncol = length(x))
    for (i in seq(Tauy)) {
      K[i, ] <- dk(x, distr = distr, mu = Tauy[i], sigma = Tauz[i])
    }
    fcondXA2 <- apply(K, 2, function(x) sum(x * pJ))
    return(fcondXA2)
  }
