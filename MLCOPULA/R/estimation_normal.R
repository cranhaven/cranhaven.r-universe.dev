#' @title Parameter estimation for a Gaussian copula.
#' @description Returns a list with the parameter 
#' value and the log likelihood value.
#' @param U values for u and v.

estimation.normal <- function(U){
  opt <- optim(par = c(0.1), fn = lv.normal, 
               U = U, control = list(fnscale = -1),
               method = "Brent",lower = -1, 
               upper = 1)
  
  values <- list(logv = opt$value, theta = opt$par)
  return(values)
}

