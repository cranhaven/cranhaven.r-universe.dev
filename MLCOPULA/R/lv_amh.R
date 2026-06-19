#' @title Log likelihood function for a AMH copula
#' @description Returns the log likelihood value.
#' @param par Copula parameter value.
#' @param U values for u and v.

lv.amh <- function(par,U){
  copula <- amhCopula(param = par, dim = 2)
  value <- sum(log(dCopula(U,copula)))
  return(value)
}

