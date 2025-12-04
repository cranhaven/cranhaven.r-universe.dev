
# simplified Gelman convergence diagnostic using Brooks & Gelman's "interval" method.

# See Brooks & Gelman (1998) General methods for monitoring convergence of iterative simulations. J Computational and Graphical Statistics, 7, 434-455. p. 441

# This follows WinBUGS in using the central 80% interval as the measure of width (WinBUGS manual p.27).

simpleRhat3d <- function(mcmc3d) {

  if(dim(mcmc3d)[2] == 1 || dim(mcmc3d)[1] < 100) {  # only 1 chain or <100 draws per chain
    Rhat <- rep(NA_integer_, dim(mcmc3d)[3])
    names(Rhat) <- dimnames(mcmc3d)[[3]]
    return(Rhat)
  }
  
  width <- function(y)
    diff(quantile(y, c(0.1, 0.9), na.rm=TRUE))

  W0 <- apply(mcmc3d, 2:3, width)           # width of individual chains
  W <- colMeans(W0)
  B <- apply(mcmc3d, 3, width)              # width of pooled chains
  Rhat <- B / W
  Rhat[is.nan(Rhat)] <- NA
  return(Rhat)
}

simpleRhat <- function(x) {
  simpleRhat3d(matTo3d(x))
}
