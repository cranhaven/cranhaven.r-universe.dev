#' Fit Pearson Type-3 distribution using the 'lmom' package
#'
#' @param sl1 sample 1st l-moment
#' @param sl2 sample 2nd l-moment
#' @param st3 sample 3rd l-moment ratio
#' @param st4 sample 4th l-moment ratio
#'
#' @return A vector of parameters as mu (location), sigma (scale), and gamma (shape).
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @importFrom lmom pelpe3
#' @export
#'
#' @examples
#' 
#' pe3_par <- fit_pe3(15, 1.7, 0.04, -0.02)
#' 
fit_pe3 <- function(sl1, sl2, st3, st4) {
  parameters <- pelpe3(c(sl1, sl2, st3, st4))
  return(parameters)
}
