#' @title Parameter estimation for a Frank copula.
#' @description Returns a list with the parameter 
#' value and the log likelihood value.
#' @param U values for u and v.

estimation.frank <- function(U){
  opt <- optim(par = c(0.1), fn =lv.frank, 
               U = U, control = list(fnscale = -1),
               method = "Brent",lower = -100, 
               upper = 100)
  
  values <- list(logv = opt$value, theta = opt$par)
  return(values)
}
