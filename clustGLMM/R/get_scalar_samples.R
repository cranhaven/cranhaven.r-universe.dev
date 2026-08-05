get_scalar_samples <-
function(mcmc, what, gspec, dimspec, yspec, 
         burnin = 0, thin = 1, 
         iterations, chains){
  if(thin < 1){
    stop("Thinning parameter has to be integer and thin >= 1.")
  }
  if(missing(chains)){chains = mcmc$chains}
  misiter = missing(iterations)

  v <- mcmc$settings[[chains[1]]][what,]
  whatLAB <- what
    
  if(missing(yspec)){
    if(v$isy){
      Ys <- c()
      if(v$ynums){Ys <- c(Ys, mcmc$Nums)}
      if(v$ypois){Ys <- c(Ys, mcmc$Pois)}
      if(v$ybins){Ys <- c(Ys, mcmc$Bins)}
      if(v$yords){Ys <- c(Ys, mcmc$Ords)}
      if(v$ycats){Ys <- c(Ys, mcmc$Cats)}
      yspec <- Ys[1]
      warning(paste0("yspec was missing, although, ", what, 
                     " is outcome-specific, changed to yspec = ", yspec, "."))
    }else{
      yspec <- NULL
    }
  }else{
    if(v$isy){
      whatLAB <- paste(whatLAB, yspec, sep = "_")
    }else{
      yspec <- NULL
      warning(paste0("yspec was given, although, ", what, 
                     " is not outcome-specific, changed to yspec = ", yspec, "."))
    }
  }
  
  if(missing(gspec)){
    if(v$gspec){
      gspec <- 1
      warning(paste0("gspec was missing, although, ", what, 
                     " is not group-specific, changed to gspec = ", gspec, "."))
    }else{
      gspec <- NULL
    }
  }else{
    if(v$gspec){
      whatLAB <- paste0(whatLAB, "(", gspec, ")")
    }else{
      gspec <- NULL
      warning(paste0("gspec was given, although, ", what, 
                     " is not group-specific, changed to gspec = ", gspec, "."))
    }
  }
  
  if(missing(dimspec)){
    # no dimension supplied = no need for supplying -> 1-dim parameter
    if(v$D > 0){
      dimspec <- rep(1, v$D)
      warning(paste0("dimspec was missing, although, ", what, 
                     " is ", v$D, "-dimensional parameter, ", 
                     "changed to dimspec = c(", paste0(dimspec, collapse=", "), ")."))
    }else{
      dimspec <- NULL
    }
  }else{
    if(v$D > 0){
      whatLAB <- paste0(whatLAB,"[",paste(dimspec, collapse = ","),"]")
    }else{
      dimspec <- NULL
      warning(paste0("dimspec was given, although, ", what, 
                     " is 0-dimensional, changed to dimspec = ", dimspec, "."))
    }
  }
  
  scalar_samples <- list()
  scalar_samples$whatLAB <- whatLAB
  scalar_samples$draws <- list()
    
  for(ch in chains){
    if(misiter){
      # iterations = seq(burnin+1, mcmc$iter, by = thin)
      # iterations <- mcmc$draws[[ch]]$m
      if(is.null(gspec)){
        iterations <- mcmc$draws[[ch]]$m
      }else{
        # to avoid NA values created by post_processing()
        iterations <- mcmc$iterations[[ch]]
      }
      iterations <- iterations[iterations > burnin]
      iterations <- iterations[seq(1, length(iterations), by=thin)]
    }
    
    ind_iterations <- is.element(mcmc$draws[[ch]]$m, iterations)    
    v <- mcmc$settings[[ch]][what,]
    
    
    ### Draws are save in a structured list
    out <- data.frame(chain = ch, m = iterations)
    if(mcmc$howsave == "list"){
      if(v$gspec){
        if(v$isy){
          # group-specific and y-specific
          if(v$D == 0){
            out$y = mcmc$draws[[ch]][[what]][[gspec]][ind_iterations, yspec]
          }
          if(v$D == 1){
            out$y = mcmc$draws[[ch]][[what]][[gspec]][[yspec]][ind_iterations,dimspec[1]]
          }
          if(v$D == 2){
            out$y = mcmc$draws[[ch]][[what]][[gspec]][[yspec]][ind_iterations,dimspec[1],dimspec[2]]
          }
        }else{
          # group-specific but not y-specific
          if(v$D == 0){
            out$y = mcmc$draws[[ch]][[what]][ind_iterations, gspec]
          }
          if(v$D == 1){
            out$y = mcmc$draws[[ch]][[what]][[gspec]][ind_iterations,dimspec[1]]
          }
          if(v$D == 2){
            out$y = mcmc$draws[[ch]][[what]][[gspec]][ind_iterations,dimspec[1],dimspec[2]]
          }
        } # end of else of if is g-specific
      }else{
        if(v$isy){
          # not group-specific but y-specific
          if(v$D == 0){
            out$y = mcmc$draws[[ch]][[what]][ind_iterations, yspec]
          }
          if(v$D == 1){
            out$y = mcmc$draws[[ch]][[what]][[yspec]][ind_iterations,dimspec[1]]
          }
          if(v$D == 2){
            out$y = mcmc$draws[[ch]][[what]][[yspec]][ind_iterations,dimspec[1],dimspec[2]]
          }
        }else{
          # neither group-specific nor y-specific
          if(v$D == 0){
            out$y = mcmc$draws[[ch]][[what]][ind_iterations]
          }
          if(v$D == 1){
            out$y = mcmc$draws[[ch]][[what]][ind_iterations,dimspec[1]]
          }
          if(v$D == 2){
            out$y = mcmc$draws[[ch]][[what]][ind_iterations,dimspec[1],dimspec[2]]
          }
        }
      } 
    } # end of if mcmc$howsave == list
  
    ### Draws are saved in large matrix
    if(mcmc$howsave == "data.frame"){
      
      if(!is.element(whatLAB, colnames(mcmc$draws[[ch]]))){
        stop(paste("Wrong specification, column ", whatLAB, 
                   " does not exist in mcmc$draws[[",ch,"]]!"))
      }
      
      out$y = mcmc$draws[[ch]][ind_iterations, whatLAB]
      
    } # end of if howsave="data.frame"
    scalar_samples$draws[[ch]] <- out
  }# end of for ch in chains

  return(scalar_samples)
}
