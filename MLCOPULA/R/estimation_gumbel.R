#' @title Parameter estimation for a Gumbel copula.
#' @description Returns a list with the parameter 
#' value and the log likelihood value.
#' @param U values for u and v.


estimation.gumbel <- function(U){
  opt <- optim(par = c(1.01), fn = lv.gumbel, 
               U = U, control = list(fnscale = -1),
               method = "Brent",lower = 1, 
               upper = 100)
  
  values <- list(logv = opt$value, theta = opt$par)
  return(values)
}

