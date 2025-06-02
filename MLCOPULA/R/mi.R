#' @title Mutual information
#' @description Returns the mutual information of a copula.
#' @param cop a copula with parameter \eqn{\theta}.

mi <- function(cop){
  value <-  integral2(int.mi, xmin = 0, xmax = 1,
                      ymin = 0, ymax = 1, cop = cop)$Q
  return(value)
}