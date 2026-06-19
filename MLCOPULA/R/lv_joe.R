#' @title Log likelihood function for a Joe copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.joe <- function(par,U){
  copula <- joeCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}