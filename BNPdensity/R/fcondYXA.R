#' Conditional posterior distribution of the latents Y
#'
#' This function simulates form the conditional posterior distribution of the
#' latents Y.
#'
#' For internal use.
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(x, distr = 1, Tau, J, sigma) {
#'   K <- matrix(NA, nrow = length(Tau), ncol = length(x))
#'   for (i in seq(Tau)) {
#'     K[i, ] <- dk(x, distr = distr, mu = Tau[i], sigma = sigma) *
#'       J[i]
#'   }
#'   pK <- prop.table(K, margin = 2)
#'   y <- apply(pK, 2, function(x) sample(Tau, size = 1, prob = x))
#'   return(y)
#' }
fcondYXA <-
  function(x, distr, Tau, J, sigma) {
    K <- matrix(NA, nrow = length(Tau), ncol = length(x))
    for (i in seq(Tau)) {
      K[i, ] <- dk(x, distr = distr, mu = Tau[i], sigma = sigma) *
        J[i]
    }
    pK <- prop.table(K, margin = 2)
    y <- apply(pK, 2, function(x) sample(Tau, size = 1, prob = x))
    return(y)
  }
