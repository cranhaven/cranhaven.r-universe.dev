plot_diagnostics <-
function(mcmc, scalar_samples, 
         what, gspec, dimspec, yspec,
         burnin = 0, thin = 1, lag.max = 30, iterations,
         chains, trueval, setparmfrow = TRUE,
         COL, move.width = 0.3, whatLAB){
  if(setparmfrow){
      opar <- par(mfrow = c(2, 2))
      on.exit(par(opar))
  }
  if(missing(chains)){chains = mcmc$chains}
  #if (missing(gspec)){gspec <- 1}
  #if (missing(yspec)){yspec <- ifelse(is.null(mcmc$Nums), mcmc$Ords[1], mcmc$Nums[1])}
  if(!missing(yspec)){
    if (!is.element(yspec, names(mcmc$family))){
      stop("Unexisting response variable specified.")
    }
  }
  if(missing(scalar_samples)){
    scalar_samples <- get_scalar_samples(mcmc = mcmc, what = what, 
                                         gspec = gspec, dimspec = dimspec, yspec = yspec,
                                         burnin = burnin, thin = thin, 
                                         iterations = iterations, chains = chains)
  }
  
  plot_traceplots(mcmc=mcmc, scalar_samples = scalar_samples, 
                  what=what, gspec=gspec, dimspec=dimspec, yspec=yspec,
                  chains=chains, COL=COL, labcex = 0.8, whatLAB=whatLAB)
  if(!missing(trueval)){
    abline(h = trueval, col = "seagreen", lty = 2)
  }
  plot_ECDF(mcmc=mcmc, scalar_samples = scalar_samples,
            what=what, gspec=gspec, dimspec=dimspec, yspec=yspec,
            chains=chains, COL=COL, labcex = 0.8, whatLAB=whatLAB)
  if(!missing(trueval)){
    abline(v = trueval, col = "seagreen", lty = 2)
  }
  plot_kerneldensity(mcmc=mcmc, scalar_samples = scalar_samples,
                     what=what, gspec=gspec, dimspec=dimspec, yspec=yspec,
                     chains=chains, COL=COL, labcex = 0.8, whatLAB=whatLAB)
  if(!missing(trueval)){
    abline(v = trueval, col = "seagreen", lty = 2)
  }
  plot_ACF(mcmc=mcmc, scalar_samples = scalar_samples,
           what=what, gspec=gspec, dimspec=dimspec, yspec=yspec,
           chains=chains, COL=COL, labcex = 0.8, whatLAB=whatLAB,
           lag.max = lag.max, move.width = move.width)
  
}
