#' @title Log likelihood function for a Gumbel copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.gumbel <- function(par,U){
  copula <- gumbelCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}

