weightEstim <- function(dat,lam=0.4,a=1,tol=1e-6){

  # Initialize the weight vector
  w.mb <- rep(1,ncol(dat))

  if (length(w.mb) == 1) ## w.mb has to be a vector
  stop("weight should be a vector whose length is equal to column size of design matrix")

  # Normalize the weights
  w.mat <- w.mb/sum(w.mb)

  # Update equation for weights
  converge = FALSE

  while (converge==FALSE){
    w.mat.old <- w.mat
    w.mat <- (1-a)*w.mat + a*weightComp(dat,lam,w.mat)
    eps <- sum((w.mat.old-w.mat)^2)

    if (eps < tol){
      converge = TRUE
    }
    w.dat <- w.mat
  }
  return(w.dat)
}
