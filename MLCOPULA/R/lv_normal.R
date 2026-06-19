#' @title Log likelihood function for a Gaussian copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.normal <- function(par,U){
  copula <- normalCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}


