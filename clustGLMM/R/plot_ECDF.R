plot_ECDF <-
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
  
  xmax <- -Inf
  xmin <- Inf
  for(ch in chains){
    scalar_samples$draws[[ch]]$y[is.infinite(scalar_samples$draws[[ch]]$y)] <- NA
    xmax <- max(c(scalar_samples$draws[[ch]]$y, xmax), na.rm = TRUE)
    xmin <- min(c(scalar_samples$draws[[ch]]$y, xmin), na.rm = TRUE)
  }
  
  if(missing(COL)){
    if(mcmc$nchains == 1){
      COL <- "grey"
    }else{
      COL <- diverge_hcl(mcmc$nchains, c = 80, l =70)
    }
  }
  
  plot(x = 1, y = 1, type = "n", xlim = c(xmin, xmax), ylim = c(0,1),
       xlab = "", ylab = "")
  mtext(text = whatLAB, side = 1, line = 2.5, cex = labcex)
  mtext(text = "ECDF", side = 2, line = 2.5, cex = labcex)
  for(ch in chains){
    plot(ecdf(scalar_samples$draws[[ch]]$y), col = COL[ch], add = T, verticals = T, pch = NA)
  }
}
