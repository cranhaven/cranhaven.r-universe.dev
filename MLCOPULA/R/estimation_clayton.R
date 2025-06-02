#' @title Parameter estimation for a Clayton copula.
#' @description Returns a list with the parameter 
#' value and the log likelihood value.
#' @param U values for u and v.

estimation.clayton <- function(U){
  suppressWarnings(
  opt <- optim(par = c(0.1), fn = lv.clayton, 
               U = U, control = list(fnscale = -1),
               method = "Brent",lower = -1, 
               upper = 100))
  
  values <- list(logv = opt$value, theta = opt$par)
  return(values)
}
