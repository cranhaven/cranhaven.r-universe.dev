from_matrix_to_coda <- function(mcmc, burnin=0, thin=1){
  codamcmc <- list()
  # There is slight possibility that it will produce some NA values
  # We also omit n allocation indicators U
  for(ch in mcmc$chains){
    codamcmc[[ch]] <- sapply(mcmc$draws[[ch]], as.numeric)
    codamcmc[[ch]] <- codamcmc[[ch]][mcmc$draws[[ch]]$m > burnin, ]
    codamcmc[[ch]] <- codamcmc[[ch]][seq(1, dim(codamcmc[[ch]])[1], by = thin),-1,drop=FALSE]
    if(dim(mcmc$draws[[ch]])[1] == mcmc$iter){
      # We work with original chain of length mcmc$iter
      codamcmc[[ch]] <- mcmc(codamcmc[[ch]], 
                             thin = thin, start = burnin+1, end = mcmc$iter)
    }else{
      # We work with a subset of a chain (restriction to Gplus, clustering probabilities, ...)
      # Start numbering from 1 and omit any information about thinning 
      codamcmc[[ch]] <- mcmc(codamcmc[[ch]], 
                             thin = 1, start = 1, end = dim(mcmc$draws[[ch]])[1])
    }
  }
  #
  out <- try(as.mcmc.list(codamcmc), silent = TRUE)
  if(is(out, "try-error")){
    # cannot coerce to mcmc.list
    message("Cannot coerce to mcmc.list because of different number of variables in each chain. 
Only a list of mcmc objects is returned, apply summary(), etc. only per chain.")
  }else{
    codamcmc <- out
  }
  return(codamcmc)
}
