#' @title Returns dependency measures for a grid type copula
#' @return A list of calculated measurements
#' @param gc a grid type copula object.
#' @param measures A vector of the measurements to calculate: 
#' "gini", "blomqvist", "tail_U", "tail_L", "rho", "tau","mi",
#' by default "all".
#' @example 
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 10
#' m <- 10
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' measures.grid(gc = copula.grid,measures = c("rho","tau"))
#' @export


measures.grid <- function(gc,measures = "all") {
  measures_f <- list(
    "gini" = gini.grid,
    "blomqvist" = blomqvist.grid,
    "tail_U" = tailU.grid,
    "tail_L" = tailL.grid,
    "rho" = rho.grid,
    "tau" = tau.grid,
    "mi" = mi.grid
  )

  if(measures[1] != "all"){
    measures_f <- measures_f[measures]
  }
  
  values <- lapply(measures_f, function(f) f(gc))
  
  return(values)
}

