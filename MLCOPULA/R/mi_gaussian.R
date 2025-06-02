mi.gaussian <- function(rho){
  #rho <- copula@parameters
  mi <- (-1/2) * log(1 - rho^2)
  return(mi)
}
