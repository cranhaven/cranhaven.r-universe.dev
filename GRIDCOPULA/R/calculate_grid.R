#' @title Estimates the parameters of a grid type copula according to their log-likelihood and a given data set
#' @description Returns a list with a matrix with the density over the grid,
#' a matrix with the quantity of data over the grid, the number of subintervals for the U2 variable,
#' the number of subintervals for the U1 variable.
#' @param U a matrix of size nx2 with the observed values.
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
#' @param D.ini a matrix with the initial values for the density copula.package: the name of the package for numerical optimization.
#' @examples






calculate.ml <- function(U, k, m, D.ini=NULL) {
  Qm <- count.grid(U, k, m)
  Dm <- matrix(1, nrow=k, ncol=m)
  
  if(is.null(D.ini)) {
    x0 <- rep(1,m*k)
  } else {
    	if(validate.density(D.ini,k,m)==TRUE)
	{
	x0 <- as.vector(D.ini)
	}else
	{
	x0 <- rep(1,m*k)
	warning("D.inin is not a valid density matrix, it was replaced by other values")
	}
  }
  
  # Rsolnp package
  value <- solnp(pars=x0, fun=objective.grid, eqfun=equalities.grid,
                 eqB=c(rep(k,m),rep(m,(k-1))), LB=rep(0,m*k), UB=rep(min(k,m),m*k), A=Qm)
  Dm <- matrix(value$pars, nrow=k, ncol=m)
  
  return(list(Density=Dm, Quantity=Qm, m=m, k=k))
}

