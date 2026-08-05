plot_clusters <-
function(mcmc, 
         what, dimspec, yspec, whichg,
         burnin = 0, thin = 1, iterations, chains, 
         setparmfrow = TRUE, doKern = TRUE, doECDF = TRUE,
         COL, labcex = 0.8, whatLAB, wherelegend = "topright"){
  if(setparmfrow){
    opar <- par(mfrow = c(1, sum(c(doKern, doECDF))))
    on.exit(par(opar))
  }
  if(is.element(what,names(mcmc$varying))){
    if(!mcmc$varying[what]){stop("This parameter wasn't considered as cluster-specific.")}
  }
  if(missing(chains)){chains <- mcmc$chains}
  # if(missing(chains)){
  #   if(mcmc$post_processed){
  #     chains <- mcmc$chains[1]
  #   }else{
  #     chains <- mcmc$chains
  #   }
  # }else{
  #   if(mcmc$post_processed & length(chains) > 1){
  #     warning("It is recommended to use one chain only when working with post-processed samples, 
  #             only first element of chains will be used.")
  #     chains <- chains[1]
  #   }
  # }
    
  #if (missing(yspec)){yspec <- ifelse(is.null(mcmc$Nums), mcmc$Ords[1], mcmc$Nums[1])}
  if(!missing(yspec)){
    if(!is.element(yspec, names(mcmc$family))){
      stop("Unexisting response variable specified.")
    }
  }
  if(missing(whichg)){
    sameclusters <- TRUE
    for(ch in chains[-1]){
      sameclusters <- sameclusters & setequal(mcmc$clusters[[chains[1]]], 
                                              mcmc$clusters[[ch]])
    }
    if(sameclusters){
      whichg <- mcmc$clusters[[chains[1]]]
    }else{
      stop("You are trying to plot multiple chains which differ in mcmc$clusters, default for whichg.
           Either select whichg manually or use one chain only.")
    }
  }
  
  scalar_samples <- data.frame()
  for(ch in chains){
    for(g in whichg){
      aux <- get_scalar_samples(mcmc = mcmc, what = what,
                                gspec = g, dimspec = dimspec, yspec = yspec,
                                burnin = burnin, thin = thin, 
                                iterations = iterations, chains = ch)
      if(g == whichg[1]){
        colnames(aux$draws[[ch]])[which(colnames(aux$draws[[ch]])=="y")] <- paste0("y(",g,")")
        ssch = aux$draws[[ch]]
      }else{
        ssch[,paste0("y(",g,")")] <- aux$draws[[ch]]$y
      }
    }
    scalar_samples <- rbind(scalar_samples, ssch)
  }

  
  if(missing(whatLAB)){
    whatLAB <- what
    
    if(!missing(yspec)){
      whatLAB <- paste(whatLAB, yspec, sep = "_")
    }
    
    if(missing(dimspec)){
      # no dimension supplied = no need for supplying -> 1-dim parameter
    }else{
      whatLAB <- paste0(whatLAB,"[",paste(dimspec, collapse=","),"]")
    }
  }
  if(missing(COL)){COL <- rainbow_hcl(mcmc$G[chains[1]], c = 80, l =70)}
  
  
  # Density
  if(doKern){
    kernest <- data.frame()
    for(g in whichg){
      dd <- density(scalar_samples[, paste0("y(",g,")")], na.rm = T)
      kernest <- rbind(kernest, data.frame(x=dd$x, y=dd$y, cluster = g))
    }
    plot(c(min(kernest$y),max(kernest$y)) ~ quantile(kernest$x, probs = c(0.03,0.97)), type = "n",
         xlab = "", ylab = "")
    mtext(text = whatLAB, side = 1, line = 2.5, cex = labcex)
    mtext(text = "Density", side = 2, line = 2.5, cex = labcex)
    for(g in whichg){
      lines(kernest$y[kernest$cluster==g] ~ kernest$x[kernest$cluster==g],
            col = COL[g], lty = 1, lwd = 2)
    }
    legend(wherelegend, legend = whichg,
           col = COL[whichg], lty =1, bty = "n", title = "Cluster", cex = 0.8)
  }
  # ECDF
  if(doECDF){
    plot(x = 1, y = 1, type = "n", 
         xlim = c(min(scalar_samples[,grep("y",colnames(scalar_samples))]), 
                  max(scalar_samples[,grep("y",colnames(scalar_samples))])), 
         ylim = c(0,1),
         xlab = "", ylab = "")
    mtext(text = whatLAB, side = 1, line = 2.5, cex = labcex)
    mtext(text = "ECDF", side = 2, line = 2.5, cex = labcex)
    for(g in whichg){
      plot(ecdf(scalar_samples[,paste0("y(", g,")")]), 
           col = COL[g], add = TRUE, verticals = TRUE, pch = NA)
    }
  }
}
