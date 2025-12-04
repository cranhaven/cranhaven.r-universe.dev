
# Bayes version of single-season occupancy model with no covariates

# This version uses a Gibbs sampler coded in R

BoccSS0 <- function(y, n, psiPrior=c(1,1), pPrior=c(1,1),
                    chains=3, draws=30000, burnin=100, ...) {
  startTime <- Sys.time()

  if(!is.null(list(...)$sample)) {
    message("* The 'sample' argument is deprecated, please use 'draws'. *")
    draws <- list(...)$sample
  }

  if(!is.null(dim(y)) && dim(y)[2] > 1) {  # detection history
    n <- rowSums(!is.na(y))
    y <- rowSums(y, na.rm=TRUE)
  }
  nSites <- length(y)
  if(length(n) == 1)
    n <- rep(n, nSites)
  if(length(n) != length(y))
    stop("Lengths of 'y' and 'n' must be equal.")
  if(any(y > n))
    stop("No value of 'y' can be greater than the corresponding 'n'.")
  known <- y > 0  # sites known to be occupied
  detected <- sum(y)

  n.iter <- ceiling(draws / chains) + burnin
  chain <- matrix(nrow=n.iter, ncol=2)
  colnames(chain) <- c("psi", "p")
  chain[1, ] <- rep(0.5, 2)
  chainList <- vector('list', chains)
  for(ch in 1:chains)  {
    for(i in 2:n.iter) {
      # 1. Calculate prob(occupied | y = 0), draw new z vector
      psi.y0 <- (chain[i-1,1] * (1 - chain[i-1,2])^n) /
                    (chain[i-1,1] * (1 - chain[i-1,2])^n + (1 - chain[i-1,1]))
      z <- ifelse(known, 1, rbinom(nSites, 1, psi.y0))
      # 2. Update psi from beta(occupied+1, unoccupied+1)
      chain[i,1] <- rbeta(1, sum(z) + psiPrior[1], sum(z == 0) + psiPrior[2])
      # 3. Update p from beta(detected+1, undetected+1) for occupied sites only.
      chain[i,2] <- rbeta(1, detected + pPrior[1], sum(n[z == 1]) - detected + pPrior[2])
    }
    chainList[[ch]] <- mcmc(chain[(burnin+1):n.iter, ])
  }
  # Diagnostics
  MCMC <- mcmc.list(chainList)

  out <- mcmcOutput(MCMC,
      header = "Model fitted in R with a Gibbs sampler")
  attr(out, "call") <- match.call()
  attr(out, "timeTaken") <- unclass(difftime(Sys.time(), startTime, units="secs"))
  return(out)
}
