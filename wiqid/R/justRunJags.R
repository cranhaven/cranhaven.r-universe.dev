
# Functions to run JAGS via 'rjags' with no extra features/annoyances

# loads/unloads more than one JAGS module

# rjags::load.module and unload.module will only load/unload one module!
loadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::load.module(modules[i])
}

unloadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::unload.module(modules[i])
}
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# Run JAGS in serial mode.

# This function is also called (with chains=1) to run JAGS in each worker.
# Note that initList MUST be the first argument to work with parLapply.
justRunJagsSerial <- function(initList, data, params, modelFile,
    chains, draws, burnin, adapt=1000, thin=1) {
  jm <- rjags::jags.model(modelFile, data, initList, n.chains=chains, n.adapt=adapt)
  if(burnin > 0)
    update(jm, burnin)
  rjags::coda.samples(jm, params, n.iter=ceiling(draws / chains) * thin, thin=thin)
}
# ---------------------------------------------------------------

# The main function to run JAGS

justRunJags <- function(data, inits, params, modelFile,
        chains, draws, burnin, thin=1, adapt = 1000,
        modules = c("glm"), parallel = NULL, seed=NULL)  {

  # Check that `rjags` is installed
  if(!requireNamespace("rjags", quietly=TRUE)) {
    stop("The 'rjags' package and the JAGS program are needed to run this function.",
      call.=FALSE)
  }
  # Deal with parallelism:
  if(chains == 1)
    parallel <- FALSE
  if(is.null(parallel))
    parallel <- chains < detectCores()
  if(parallel) {
    coresToUse <- min(chains, detectCores() - 1)
    if(coresToUse < 2) {
      warning("Multiple cores not available; running chains sequentially.")
      parallel <- FALSE
    }
  }
  if(parallel) {
    if(chains > coresToUse)
      warning(paste("Running", chains, "chains on", coresToUse, "cores."))
  }

  # Deal with seeds and RNGs
  set.seed(seed, kind='default')
  chainSeeds <- sample.int(1e6, chains)
  rng0 <- paste("base", c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
    "Mersenne-Twister"), sep="::")
  rng <- rep(rng0, length=chains)

  # Fix inits
  if(is.function(inits))  {
    initList <- lapply(1:chains, function(x) inits())
  } else if (is.list(inits) && length(inits) == chains) {
    initList <- inits
  } else stop("inits must be a function or a list of length = chains")
  for(i in 1:chains) {
    initList[[i]]$.RNG.name <- rng[i]
    initList[[i]]$.RNG.seed <- chainSeeds[i]
  }

  if(parallel) {   ##### Do the parallel stuff #####
    message("Waiting for parallel processing to complete...", appendLF=FALSE) ; flush.console()
    cl <- makeCluster(coresToUse) ; on.exit(stopCluster(cl))
    clusterEvalQ(cl, library(rjags))
    if(!is.null(modules)) {
      clusterExport(cl, c("modules", "loadJagsModules"), envir=environment())
      clusterEvalQ(cl, loadJagsModules(modules)) # No need to unload as we stopCluster
    }
    chainList <- parLapply(cl, initList, justRunJagsSerial, data=data, params=params,
      modelFile=modelFile, chains=1, draws=ceiling(draws / chains), burnin=burnin, adapt=adapt, thin=thin)
    mcmcList <- mcmc.list(lapply(chainList, function(x) x[[1]]))
    message("done.")
  } else {     ##### Do the serial stuff #####
    if(!is.null(modules))
      loadJagsModules(modules)
    mcmcList <- justRunJagsSerial(initList, data=data, params=params, modelFile=modelFile,
                  chains=chains, draws=draws, burnin=burnin, adapt=adapt, thin=thin)
    if(!is.null(modules))
      unloadJagsModules(modules)
  }

  invisible(mcmcList)
}
