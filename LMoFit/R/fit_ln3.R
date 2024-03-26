#' Fit LogNormal-3 distribution using the 'lmom' package
#'
#' @param sl1 sample 1st l-moment
#' @param sl2 sample 2nd l-moment
#' @param st3 sample 3rd l-moment ratio
#' @param st4 sample 4th l-moment ratio
#'
#' @return A vector of parameters as zeta (lower bound), mu (mean on log-scale), and sigma (st.dev. on log-scale)
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @importFrom lmom pelln3
#' @export
#'
#' @examples
#' 
#' ln3_par <- fit_ln3(15, 1.7, 0.04, -0.02)
#' 
fit_ln3 <- function(sl1, sl2, st3, st4) {
  parameters <- pelln3(c(sl1, sl2, st3, st4))
  return(parameters)
}
