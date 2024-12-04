DWLasso <- function(X,lambda1=0.4,lambda2=2,a=1,tol=1e-5){

  if(ncol(X) == 1 | nrow(X) == 1)
    stop("the data should be a matrix")

  if(lambda1 < 0 | lambda2 < 0)
    stop("penalty parameter should be positive")

  if(length(lambda1) > 1 | length(lambda2) > 1)
    stop("penalty parameter should be a scalar")

  if(tol <= 0)
    stop("tolerance should be positive")

  if(length(tol) > 1)
    stop("tolerance should be a scalar")

  if(a < 0)
    stop("the parameter cannot be negative")

  if(length(a) > 1)
    stop("the parameter should be a scalar")


# Estimate weights
w.est <- weightEstim(X, lam=lambda1, a=1, tol=1e-5)

# Reconstruct the final graph
adj.mat <- MBLasso(X, lambda = lambda2, w.est)

adj.out <- list(mat = adj.mat, weights = w.est, lambda1=lambda1, lambda2=lambda2)

return(adj.out)
}
