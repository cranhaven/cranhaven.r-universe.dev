#' @title Parameter estimation for a AMH copula.
#' @description Returns a list with the parameter 
#' value and the log likelihood value.
#' @param U values for u and v.


estimation.amh <- function(U){
  opt <- optim(par = c(1.01), fn = lv.amh, 
               U = U, control = list(fnscale = -1),
               method = "Brent",lower = -1, 
               upper = 1)
  
  values <- list(logv = opt$value, theta = opt$par)
  return(values)
}

