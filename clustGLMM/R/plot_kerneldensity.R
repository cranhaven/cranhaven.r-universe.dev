plot_kerneldensity <-
function(mcmc, scalar_samples, 
         what, gspec, dimspec, yspec,
         burnin = 0, thin = 1, iterations, chains,
         dolegend = FALSE,
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
                                         iterations= iterations, chains = chains)
  }
  if(missing(whatLAB)){
    whatLAB <- scalar_samples$whatLAB
  }
  if(missing(COL)){
    if(mcmc$nchains == 1){
      COL <- "grey"
    }else{
      COL <- diverge_hcl(mcmc$nchains, c = 80, l =70)
    }
  }
  
  kernest <- data.frame()
  for(ch in chains){
    dd <- density(scalar_samples$draws[[ch]]$y, na.rm = TRUE)
    kernest <- rbind(kernest, data.frame(x=dd$x, y=dd$y, chain = ch))
  }
  plot(c(min(kernest$y),max(kernest$y)) ~ quantile(kernest$x, probs = c(0.03,0.97)), type = "n",
       xlab = "", ylab = "")
  mtext(text = whatLAB, side = 1, line = 2.5, cex = labcex)
  mtext(text = "Density", side = 2, line = 2.5, cex = labcex)
  for(ch in chains){
    lines(kernest$y[kernest$chain==ch] ~ kernest$x[kernest$chain == ch],
          col = COL[ch], lty = 1, lwd = 2)
  }
  if(dolegend){
    legend("topright", legend = chains,
          col = COL[chains], lty =1, bty = "n", title = "Chain", cex = 0.8)
  }
  
}
