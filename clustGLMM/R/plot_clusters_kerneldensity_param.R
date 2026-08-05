plot_clusters_kerneldensity_param <-
  function(mcmc, what, whichg, yspec, burnin = 0, thin = 1, iterations, chains, setparmfrow=TRUE){

    if (setparmfrow) {  
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
    }
        
    if(missing(chains)){chains = mcmc$chains}
    if(!is.element(what, rownames(mcmc$settings[[chains[1]]]))){
      stop(paste0("Parameter ", what, " does not exist, check spelling or the rows of setting matrix for possible values."))
    }
    v <- c(mcmc$settings[[chains[1]]][what, ])
    # assuming all settings are the same for all chains... (G and dimswithG not used here)

    Ys <- c()
    if(v$ynums){Ys <- c(Ys, mcmc$Nums)}
    if(v$ypois){Ys <- c(Ys, mcmc$Pois)}
    if(v$ybins){Ys <- c(Ys, mcmc$Bins)}
    if(v$yords){Ys <- c(Ys, mcmc$Ords)}
    if(v$ycats){Ys <- c(Ys, mcmc$Cats)}
    
    if (!missing(yspec)) {
      intYs <- intersect(yspec, Ys)
      if(length(intYs) == 0){
        warning(paste0("The given yspec does not contain any outcome names suitable for parameter ", what, ".  
                       Continuing with all suitable ones. If smaller subset is desired, please, change yspec appropriately."))
      }else{
        Ys <- intYs
      }
    }
    
    if(missing(whichg)){
      whichg <- mcmc$clusters
    }else{
      if(!is.list(whichg)){
        gs <- whichg
        whichg <- list()
        for(ch in chains){
          whichg[[ch]] <- gs
        }
      }
    }
    opar0 <- par(mar = c(4, 4, 1, 1))
    on.exit(par(opar0), add = setparmfrow)
    
    if(v$dims > 0){  
      if(v$sym){
        if(v$diag){
          if(v$gspec){
            # symmetric matrix with diagonal is group-specific
            for(ch in chains){
              if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
              for(i in 1:v$d1){
                for(j in 1:v$d2){
                  plot_clusters(mcmc = mcmc, what = what, 
                                dimspec = c(i,j), whichg = whichg[[ch]], 
                                burnin = burnin, thin = thin, 
                                iterations = iterations, chains = ch,
                                setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                }
              }
            }
          }else{
            # symmetric matrix with diagonal is not group-specific
            warning(paste0("Parameter ", what, " is not group-specific."))
          }
        }else{
          if(v$gspec){
            # symmetric matrix withOUT diagonal is group-specific
            for(ch in chains){
              if(setparmfrow){par(mfrow = c(v$d1-1, v$d2-1))}
              for(i in 1:(v$d1-1)){
                for(j in 2:v$d2){
                  if(i < j){
                    plot_clusters(mcmc = mcmc, what = what, 
                                  dimspec = c(i,j), whichg = whichg[[ch]], 
                                  burnin = burnin, thin = thin, 
                                  iterations = iterations, chains = ch,
                                  setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                  labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                  }else{
                    plot(x=c(0,1), y=c(0,1), type = "n", xlab = "", ylab = "",
                         xaxt = "n", yaxt = "n", bty = "n")
                  }
                }
              }
            }
          }else{
            # symmetric matrix withOUT diagonal is not group-specific
            warning(paste0("Parameter ", what, " is not group-specific."))
          }
        }
      }else{
        # parameter is not a symmetric matrix
        if(v$gspec){
          # group-specific parameter
          if(v$D == 0){
            # zero-dimensional parameter
            if(v$isy){
              # y-specific 
              if(setparmfrow){par(mfrow = c(length(chains), length(Ys)))}
              for(ch in chains){
                for(y in Ys){
                  plot_clusters(mcmc = mcmc, what = what, 
                                yspec = y, whichg = whichg[[ch]],
                                burnin = burnin, thin = thin, 
                                iterations = iterations, chains = ch,
                                setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                labcex = ifelse(length(chains)*length(Ys) > 1, 0.6, 1.0))
                }
              }
            }else{
              # not y-specific, but still g-specific
              if(setparmfrow){par(mfrow = nice_nrow_ncol(length(chains)))}
              for(ch in chains){
                plot_clusters(mcmc = mcmc, what = what,
                              whichg = whichg[[ch]],
                              burnin = burnin, thin = thin, 
                              iterations = iterations, chains = ch,
                              setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                              labcex = ifelse(length(chains) > 1, 0.6, 1.0))
              }
            }
          }
          
          if(v$D == 1){
            # one-dimensional parameter
            if(v$ydepd1){
              # the size of the dimension depends on outcomes
              for(ch in chains){
                for(y in Ys){
                  if(setparmfrow){par(mfrow = nice_nrow_ncol(mcmc$yspecd1[[what]][[y]]))}
                  for(i in 1:mcmc$yspecd1[[what]][[y]]){
                    plot_clusters(mcmc = mcmc, what = what, 
                                  yspec = y, dimspec = i, whichg = whichg[[ch]],
                                  burnin = burnin, thin = thin, 
                                  iterations = iterations, chains = ch,
                                  setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                  labcex = 0.6)
                  }
                }
              }
            }else{
              # the size of the dimension depends on outcomes
              # par(mfrow = c(length(chains), v$d1))}
              if(setparmfrow){par(mfrow = nice_nrow_ncol(length(chains)*v$d1))}
              for(ch in chains){
                for(i in 1:v$d1){
                  plot_clusters(mcmc = mcmc, what = what, 
                                dimspec = i, whichg = whichg[[ch]],
                                burnin = burnin, thin = thin, 
                                iterations = iterations, chains = ch,
                                setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                labcex = 0.6)
                }
              }
            }
          }
          
          if(v$D == 2){
            # two-dimensional parameter
            if((v$ydepd1) | (v$ydepd2)){
              # some size of the dimension depends on outcomes
              for(ch in chains){
                for(y in Ys){
                  if(v$ydepd1){
                    d1 = mcmc$yspecd1[[what]][[y]]
                  }else{
                    d1 = v$d1
                  }
                  if(v$ydepd2){
                    d2 = mcmc$yspecd2[[what]][[y]]
                  }else{
                    d2 = v$d2
                  }
                  if(setparmfrow){par(mfrow = c(d1,d2))}
                  for(i in 1:d1){
                    for(j in 1:d2){
                      plot_clusters(mcmc = mcmc, what = what, 
                                    yspec = y, dimspec = c(i,j), whichg = whichg[[ch]],
                                    burnin = burnin, thin = thin, 
                                    iterations = iterations, chains = ch,
                                    setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                    labcex = 0.6)
                    }
                  }
                }
              }
            }else{
              # none dimension is y-specific
              for(ch in chains){
                if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
                for(i in 1:v$d1){
                  d2 <- ifelse(v$d2spec, mcmc$d2spec[[what]][i], v$d2)
                  for(j in 1:d2){
                    plot_clusters(mcmc = mcmc, what = what, 
                                  dimspec = c(i,j), whichg = whichg[[ch]],
                                  burnin = burnin, thin = thin, 
                                  iterations = iterations, chains = ch,
                                  setparmfrow = FALSE, doKern = TRUE, doECDF = FALSE, 
                                  labcex = 0.6)
                  }
                  if(d2 < v$d2){
                    for(j in 1:(v$d2-d2)){
                      plot(0,0, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
                    }
                  }
                }
              }
            }
          }
          
        }else{
          # parameter is not group-specific 
          warning(paste0("Parameter ", what, " is not group-specific."))
        }
      }
    }else{
      warning("There is nothing to be plotted, parameter dimension is 0.")
    }
    
  }
