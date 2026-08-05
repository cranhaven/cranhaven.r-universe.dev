from_list_to_matrix <- function(mcmc){
  params <- names(mcmc$save[mcmc$save])
  draws <- mcmc$draws
  retmcmc <- mcmc
  
  # insert the samples 
  for(ch in mcmc$chains){
    iters <- draws[[ch]]$m
    nparams <- sum(mcmc$settings[[ch]][mcmc$settings[[ch]]$save, "dimswithG"])
    retmcmc$draws[[ch]] <- matrix(NA_real_, nrow = length(iters), ncol = 1+nparams)
    retmcmc$draws[[ch]][,1] <- iters
    colnames(retmcmc$draws[[ch]]) <- c("m", unlist(mcmc$param_names[[ch]]))
    retmcmc$draws[[ch]] <- as.data.frame(retmcmc$draws[[ch]])
    # retmcmc$draws[[ch]] <- data.frame(m = iters) # but larger every time
  
    for(p in params){
      # transfer from list to C
      cmcmc <- from_list_to_C(draws = draws[[ch]],
                              p = p,
                              settings = mcmc$settings[[ch]],
                              yspecd1 = mcmc$yspecd1[[p]],
                              yspecd2 = mcmc$yspecd2[[p]],
                              d2spec = mcmc$d2spec[[p]],
                              family = mcmc$family)
      # transfer from C to matrix
      aux <- from_C_to_matrix(values = cmcmc,
                              p = p,
                              settings = mcmc$settings[[ch]],
                              yspecd1 = mcmc$yspecd1[[p]],
                              yspecd2 = mcmc$yspecd2[[p]],
                              d2spec = mcmc$d2spec[[p]],
                              family = mcmc$family)
      retmcmc$draws[[ch]][, colnames(aux)] <- aux
    }
    # chain ch completed
  }
  
  retmcmc$howsave <- "data.frame"
  class(retmcmc) <- "clustglmm"
  
  return(retmcmc)
}
