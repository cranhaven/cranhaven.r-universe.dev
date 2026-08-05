plot_traceplots <-
function(mcmc, scalar_samples, 
         what, gspec, dimspec, yspec,
         burnin = 0, thin = 1, iterations, chains,
         COL, labcex = 1, whatLAB){
  opar <- par(mar = c(3.5, 3.5, 0.8, 0.8))
  on.exit(par(opar))
  
  if(missing(chains)){chains = mcmc$chains}
  #if (missing(gspec)){gspec <- 1}
  if(!missing(yspec)){if(!is.element(yspec, names(mcmc$family))){stop("Unexisting response variable specified.")}}
  #if (missing(yspec)){yspec <- ifelse(is.null(mcmc$Nums), mcmc$Ords[1], mcmc$Nums[1])}
  #if (missing(yspec)){yspec <- ""}
  if(missing(scalar_samples)){
    scalar_samples <- get_scalar_samples(mcmc = mcmc, what = what, 
                                         gspec = gspec, dimspec = dimspec, yspec = yspec,
                                         burnin = burnin, thin = thin, 
                                         iterations = iterations, chains = chains)
  }
  if(missing(whatLAB)){
    whatLAB <- scalar_samples$whatLAB
  }
  
  ymax <- xmax <- -Inf
  ymin <- xmin <- Inf
  for(ch in chains){
    scalar_samples$draws[[ch]]$y[is.infinite(scalar_samples$draws[[ch]]$y)] <- NA
    ymax <- max(c(scalar_samples$draws[[ch]]$y, ymax), na.rm = TRUE)
    ymin <- min(c(scalar_samples$draws[[ch]]$y, ymin), na.rm = TRUE)
    xmax <- max(c(scalar_samples$draws[[ch]]$m, xmax), na.rm = TRUE)
    xmin <- min(c(scalar_samples$draws[[ch]]$m, xmin), na.rm = TRUE)
  }
  
  if(missing(COL)){
    if(mcmc$nchains == 1){
      COL <- "grey"
    }else{
      COL <- diverge_hcl(mcmc$nchains, c = 80, l =70)
    }
  }
  
  plot(c(ymin, ymax) ~c(xmin, xmax), 
       ylim = c(ymin-0.05*(ymax-ymin), ymax+0.05*(ymax-ymin)),
       ylab = "", xlab = "", type= "n")
  mtext(text = "Iteration", side = 1, line = 2.5, cex = labcex)
  mtext(text = whatLAB, side = 2, line = 2.5, cex = labcex)
  for(ch in chains){
    lines(y ~ m, data = scalar_samples$draws[[ch]], col = COL[ch])
  }
  # legend
  #legend("top", legend = chains, ncol = length(chains),
  #       col = COL[chains], lty =1, bty = "n", title = "Chain", cex = 0.8)
}
