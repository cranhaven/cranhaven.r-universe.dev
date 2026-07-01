#' runif_on_sphere
#'
#' This function generates uniform samples on a sphere in arbitrary dimension.
#'
#' @param n Sample size
#' @param d Dimension of the space
#' @param r Radius of the sphere
runif_on_sphere<-function (n, d, r = 1){
  sims <- matrix(rnorm(n * d), nrow = n, ncol = d)
  r * sims/sqrt(apply(sims, 1L, crossprod))
}
#' @noRd
#' @keywords internal
