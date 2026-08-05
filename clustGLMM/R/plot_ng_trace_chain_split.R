plot_ng_trace_chain_split <- function(mcmc, 
                                      burnin = 0, thin = 1, iterations, chains,
                                      COL, labcex = 1, setparmfrow = TRUE){
  if(!inherits(mcmc, "clustglmm")){
    stop("mcmc is not 'clustglmm' object")
  }
  
  if(missing(chains)){chains = mcmc$chains}
  if(setparmfrow){
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow = nice_nrow_ncol(length(chains)))
  }
  
  if(missing(COL)){
    if(mcmc$nchains == 1){
      COL <- "grey"
    }else{
      COL <- diverge_hcl(mcmc$nchains, c = 80, l =70)
    }
  }
  
  ngdraws <- iters <- list()
  for(ch in chains){
    if(missing(iterations)){
      iters[[ch]] <- mcmc$draws[[ch]]$m
      iters[[ch]] <- iters[[ch]][iters[[ch]] > burnin]
      iters[[ch]] <- iters[[ch]][seq(1, length(iters[[ch]]), by=thin)]
    }else{
      iters[[ch]] <- iterations
    }
    if(mcmc$howsave == "data.frame"){
      ngdraws[[ch]] <- mcmc$draws[[ch]][iterations, paste0("ng(", 1:mcmc$G[ch], ")"), drop = FALSE]
    }
    if(mcmc$howsave == "list"){
      ngdraws[[ch]] <- mcmc$draws[[ch]]$ng[iterations,]
    }
  }
  
  MIN <- min(sapply(chains, 
                    function(ch){min(ngdraws[[ch]], na.rm = TRUE)}))
  MAX <- max(sapply(chains, 
                    function(ch){max(ngdraws[[ch]], na.rm = TRUE)}))
  
  opar0 <- par(mar = c(3.5, 3.5, 0.8, 0.8))
  on.exit(par(opar0), add = setparmfrow)
  
  for(ch in chains){
    plot(0, 0, type = "n", 
         xlim = range(iters[[ch]]), ylim = c(MIN, MAX),
         xlab = "", ylab = "")
    mtext(text = "Iteration", side = 1, line = 2.5, cex = labcex)
    mtext(text = "Cluster occupancy number ng", side = 2, line = 2.5, cex = labcex)
    for(g in 1:mcmc$G[ch]){
      lines(ngdraws[[ch]][,g] ~ iters[[ch]],
            lty = 1, lwd = 1, col = COL[ch])
    }
  }
  
}
