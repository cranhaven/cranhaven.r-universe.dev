from_matrix_to_list <- function(mcmc){
  params <- names(mcmc$save[mcmc$save])
  draws <- mcmc$draws
  cmcmc <- list()
  retmcmc <- mcmc
  retmcmc$draws <- list()

  for(ch in mcmc$chains){
    iterations <- draws[[ch]]$m
    cmcmc <- chain <- list()
    chain$m <- iterations
    for(p in params){
      if(mcmc$settings[[ch]][p, "save"] & mcmc$settings[[ch]][p,"dims"] > 0){
        # First from matrix to C
        cmcmc[[p]] <- from_matrix_to_C(draws[[ch]], iterations, p, mcmc$settings[[ch]])
        
        # From C to list
        chain[[p]] <- from_C_to_list(values = cmcmc[[p]],
                                     p = p,
                                     settings = mcmc$settings[[ch]],
                                     yspecd1 = mcmc$yspecd1[[p]],
                                     yspecd2 = mcmc$yspecd2[[p]],
                                     d2spec = mcmc$d2spec[[p]],
                                     family = mcmc$family)
      }
    }
    retmcmc$draws[[ch]] <- chain
  }
  
  retmcmc$howsave <- "list"
  class(retmcmc) <- "clustglmm"
  
  return(retmcmc)

}
