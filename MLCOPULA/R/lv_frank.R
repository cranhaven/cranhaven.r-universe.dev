#' @title Log likelihood function for a Frank copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.frank <- function(par,U){
  copula <- frankCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}