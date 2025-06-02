#' @title Log likelihood function for a Clayton copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.clayton <- function(par,U){
  copula <- claytonCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}