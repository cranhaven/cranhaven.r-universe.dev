#' Fit Generalized Pareto distribution using the 'lmom' package
#'
#' @param sl1 sample 1st l-moment
#' @param sl2 sample 2nd l-moment
#' @param st3 sample 3rd l-moment ratio
#' @param st4 sample 4th l-moment ratio
#'
#' @return A vector of parameters as xi (location), alpha (scale), and k (shape).
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @importFrom lmom pelgpa
#' @export
#'
#' @examples
#' 
#' gpa_par <- fit_gpa(15, 1.7, 0.04, -0.02)
#' 
fit_gpa <- function(sl1, sl2, st3, st4) {
  parameters <- pelgpa(c(sl1, sl2, st3, st4))
  return(parameters)
}
