#' Fit Gamma distribution using the 'lmom' package
#'
#' @param sl1 sample 1st l-moment
#' @param sl2 sample 2nd l-moment
#' @param st3 sample 3rd l-moment ratio
#' @param st4 sample 4th l-moment ratio
#'
#' @return A vector of parameters as alpha (shape) and beta (scale).
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @importFrom lmom pelgam
#' @export
#'
#' @examples
#' 
#' gam_par <- fit_gam(15, 1.7, 0.04, -0.02)
#' 
fit_gam <- function(sl1, sl2, st3, st4) {
  parameters <- pelgam(c(sl1, sl2, st3, st4))
  return(parameters)
}
