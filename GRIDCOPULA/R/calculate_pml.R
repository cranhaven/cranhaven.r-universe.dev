

calculate.pml <- function(U, k, m, D.ini=NULL) {
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
  value <- solnp(pars=x0, fun=objective.pgrid, eqfun=equalities.grid,
                 eqB=c(rep(k,m),rep(m,(k-1))), LB=rep(0,m*k), 
                 UB=rep(min(k,m),m*k), A=Qm, control = list(trace = 0))
  Dm <- matrix(value$pars, nrow=k, ncol=m)

  return(list(Density=Dm, Quantity=Qm, m=m, k=k))
}

