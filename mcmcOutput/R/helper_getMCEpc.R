
# Calculation of MCMC error

# See Lunn et al 2013, The BUGS Book, p77.

# Lunn et al want number of batches = batch size = sqrt(chain length), but only
#   apply that to a single chain; they comment that with multiple chains, number of
#   batches will be larger.
# If there are many chains (eg, 20), this means number of batches >> batch size.
# To achieve number of batches = batch size, we use sqrt(total iterations),
#   corrected to be a multiple of number of chains.

# It gives the same result as coda::batchSE with an appropriate choice of batchSize.

getMCEpc3d <- function(mcmc3d, pc=TRUE) {

  ipc <- dim(mcmc3d)[1]
  nChains <- dim(mcmc3d)[2]
  nNodes <- dim(mcmc3d)[3]            # number of parameters
  postSD <- apply(mcmc3d, 3, sd)      # posterior SD

  bpc <- sqrt(ipc * nChains) %/% nChains  # batches per chain (Q)
  bsize <- floor(ipc/bpc)                 # batch size (a)
  ni <- bpc * bsize                       # number of iters per chain to use
  x <- mcmc3d[1:ni, , ]                   # discard unwanted iters
  dim(x) <- c(bsize, bpc, nChains, nNodes) # separate the batches
  bm <- apply(x, 2:4, mean)               # get batch means
  dim(bm) <- c(bpc*nChains, nNodes)        # Combine bm's across chains
  sqdev <- (sweep(bm, 2, colMeans(bm), "-"))^2  # Get squared deviation
  SD <- sqrt(colSums(sqdev) * bsize / (nChains*bpc - 1)) # sqrt(rho)
  MCEpc <- SD / sqrt(ipc * nChains)
  if(pc) {
    MCEpc <- MCEpc / postSD * 100
  } else {
    names(MCEpc) <- names(postSD)
  }
  MCEpc[is.nan(MCEpc)] <- NA
  return(MCEpc)
}

# used by 'summarise', not exported; see 'getMCE' for exported version
getMCEpc <- function(x) {
  return(getMCEpc3d(matTo3d(x), pc=TRUE))
}
