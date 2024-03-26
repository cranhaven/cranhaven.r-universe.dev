#' Estimate sample L-moments and L-moment ratios
#'
#' @param x a series of quantiles
#'
#' @return A dataframe containing the 1st l-moment, the 2nd l-moment, the 3rd l-moment, the 4th l-moment, the 2nd l-moment ratio "L-variation", the 3rd l-moment ratio "L-skewness", and the 4th l-moment ratio "L-kurtosis"
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#' @importFrom lmom samlmu
#'
#' @examples
#' 
#' sample_lmom <- get_sample_lmom((rnorm(n = 500, mean = 10, sd = 0.5)))
#' 
get_sample_lmom <- function(x) {
  if (length(x) <= 4) {stop("Sample size is small, consider larger sample")}
  lmoms <- lmom::samlmu(x, nmom = 4, ratios = FALSE)
  sl1 <- as.numeric(lmoms[1])
  sl2 <- as.numeric(lmoms[2])
  sl3 <- as.numeric(lmoms[3])
  sl4 <- as.numeric(lmoms[4])
  st2 <- sl2/sl1
  st3 <- sl3/sl2
  st4 <- sl4/sl2
  sample_lmom <- data.frame(sl1, sl2, sl3, sl4, st2, st3, st4)
  return(sample_lmom)
}
