#' Conditional posterior distribution of the distinct vectors (Ystar,Zstar)
#'
#' This function evaluates the ratio of conditional posterior distributions of
#' the distinct latent vectors (Ystar,Zstar).
#'
#' For internal use.
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(v, v2, z, z2, x, distr.k, distr.py0, mu.py0, sigma.py0, distr.pz0, mu.pz0, sigma.pz0) {
#'   alpha <- p0(v, distr = distr.py0, mu = mu.py0, sigma = sigma.py0) /
#'     p0(v2, distr = distr.py0, mu = mu.py0, sigma = sigma.py0) *
#'     p0(z, distr = distr.pz0, mu = mu.pz0, sigma = sigma.pz0) /
#'     p0(z2, distr = distr.pz0, mu = mu.pz0, sigma = sigma.pz0)
#'   Prod <- 1
#'   for (i in seq(length(x))) {
#'     fac <- dk(x[i], distr = distr.k, mu = v, sigma = z) / dk(x[i],
#'       distr = distr.k, mu = v2, sigma = z2
#'     )
#'     Prod <- Prod * fac
#'   }
#'   f <- alpha * Prod
#'   return(f)
#' }
rfyzstar <-
  function(v, v2, z, z2, x, distr.k, distr.py0, mu.py0, sigma.py0,
           distr.pz0, mu.pz0, sigma.pz0) {
    alpha <- p0(v, distr = distr.py0, mu = mu.py0, sigma = sigma.py0) / p0(v2,
      distr = distr.py0, mu = mu.py0, sigma = sigma.py0
    ) *
      p0(z, distr = distr.pz0, mu = mu.pz0, sigma = sigma.pz0) / p0(z2,
        distr = distr.pz0, mu = mu.pz0, sigma = sigma.pz0
      )
    Prod <- 1
    for (i in seq_along(x)) {
      fac <- dk(x[i], distr = distr.k, mu = v, sigma = z) / dk(x[i],
        distr = distr.k, mu = v2, sigma = z2
      )
      Prod <- Prod * fac
    }
    f <- alpha * Prod
    return(f)
  }
