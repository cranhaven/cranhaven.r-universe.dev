plot_ACF_param <-
  function(mcmc, what="ng", yspec, burnin = 0, thin = 1, iterations, lag.max = 30, chains, setparmfrow = TRUE, allclusters = FALSE){

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
    
    if(is.element(what, c("w", "ng")) | allclusters){
      nonempty <- list()
      for(ch in chains){
        nonempty[[ch]] <- 1:mcmc$G[ch] # plot all clusters, not only non-empty
      }
    }else{
      nonempty <- mcmc$clusters
    }
    sameclusters <- TRUE
    for(ch in chains[-1]){
      sameclusters <- sameclusters & setequal(nonempty[[chains[1]]], 
                                              nonempty[[ch]])
    }
    
    if(sameclusters){
      for_chains <- chains[1]
      plot_chains <- list()
      for(ch in for_chains){
        plot_chains[[ch]] <- chains
      }
    }else{
      for_chains <- chains
      plot_chains <- list()
      for(ch in for_chains){
        plot_chains[[ch]] <- ch
      }
    }
    opar0 <- par(mar = c(4, 4, 1, 1))
    on.exit(par(opar0), add = setparmfrow)
    
    if(v$dims > 0){  
      if(v$sym){
        if(v$diag){
          if(v$gspec){
            # symmetric matrix with diagonal is group-specific
            for(ch in for_chains){
              for(g in nonempty[[ch]]){
                if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
                for(i in 1:v$d1){
                  for(j in 1:v$d2){
                    if(i <= j){
                      plot_ACF(mcmc = mcmc, what = what, 
                               gspec = g, dimspec = c(i,j), 
                               burnin = burnin, thin = thin, lag.max = lag.max,
                               iterations = iterations, chains = plot_chains[[ch]],
                               labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                    }else{
                      auxdata <- get_scalar_samples(mcmc = mcmc, what = what, 
                                                    gspec = g, dimspec = c(j,i),
                                                    burnin = burnin, thin = thin, 
                                                    iterations = iterations, chains = plot_chains[[ch]])
                      plot(x=c(0,1), y=c(0,1), type = "n", xlab = "", ylab = "",
                           xaxt = "n", yaxt = "n", bty = "n")
                      means <- lapply(auxdata$draws, function(x){ifelse(is.null(x), NA, mean(x$y, na.rm=TRUE))})
                      text(0.5,0.5, labels = format(mean(unlist(means[plot_chains[[ch]]])), digits = 2, nsmall = 2))
                    }
                  }
                }
              }
            }
          }else{
            # symmetric matrix with diagonal is not group-specific
            if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
            for(i in 1:v$d1){
              for(j in 1:v$d2){
                if(i <= j){
                  plot_ACF(mcmc = mcmc, 
                           what = what, dimspec = c(i,j),  
                           burnin = burnin, thin = thin, lag.max = lag.max,
                           iterations = iterations, chains = chains,
                           labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                }else{
                  auxdata <- get_scalar_samples(mcmc = mcmc, what = what, 
                                                dimspec = c(j,i),
                                                burnin = burnin, thin = thin,
                                                iterations = iterations, chains = chains)
                  plot(x=c(0,1), y=c(0,1), type = "n", xlab = "", ylab = "",
                       xaxt = "n", yaxt = "n", bty = "n")
                  means <- lapply(auxdata$draws, function(x){ifelse(is.null(x), NA, mean(x$y, na.rm=TRUE))})
                  text(0.5,0.5, labels = format(mean(unlist(means[chains])), digits = 2, nsmall = 2))
                }
              }
            }
          }
        }else{
          if(v$gspec){
            # symmetric matrix withOUT diagonal is group-specific
            for(ch in for_chains){
              for(g in nonempty[[ch]]){
                if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
                for(i in 1:v$d1){
                  for(j in 1:v$d2){
                    if(i < j){
                      plot_ACF(mcmc = mcmc, what = what, 
                               gspec = g, dimspec = c(i,j),  
                               burnin = burnin, thin = thin, lag.max = lag.max,
                               iterations = iterations, chains = plot_chains[[ch]],
                               labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                    }else{
                      plot(x=c(0,1), y=c(0,1), type = "n", xlab = "", ylab = "",
                           xaxt = "n", yaxt = "n", bty = "n")
                      if(i == j){
                        text(0.5,0.5, labels = as.character(v$diagval))
                      }else{
                        auxdata <- get_scalar_samples(mcmc = mcmc, what = what, 
                                                      gspec = g, dimspec = c(j,i),
                                                      burnin = burnin, thin = thin, 
                                                      iterations = iterations, chains = plot_chains[[ch]])
                        means <- lapply(auxdata$draws, function(x){ifelse(is.null(x), NA, mean(x$y, na.rm=TRUE))})
                        text(0.5,0.5, labels = format(mean(unlist(means[plot_chains[[ch]]])), digits = 2, nsmall = 2))
                      }
                    }
                  }
                }
              }
            }
          }else{
            # symmetric matrix withOUT diagonal is not group-specific
            if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
            for(i in 1:v$d1){
              for(j in 1:v$d2){
                if(i < j){
                  plot_ACF(mcmc = mcmc, what = what, 
                           dimspec = c(i,j), 
                           burnin = burnin, thin = thin, lag.max = lag.max,
                           iterations = iterations, chains = chains,
                           labcex = ifelse(v$d1 <= 3, 0.6, 0.5))
                }else{
                  plot(x=c(0,1), y=c(0,1), type = "n", xlab = "", ylab = "",
                       xaxt = "n", yaxt = "n", bty = "n")
                  if(i == j){
                    text(0.5,0.5, labels = as.character(v$diagval))
                  }else{
                    auxdata <- get_scalar_samples(mcmc = mcmc, what = what, 
                                                  dimspec = c(j,i),
                                                  burnin = burnin, thin = thin, 
                                                  iterations = iterations, chains = chains)
                    means <- lapply(auxdata$draws, function(x){ifelse(is.null(x), NA, mean(x$y, na.rm=TRUE))})
                    text(0.5,0.5, labels = format(mean(unlist(means[chains])), digits = 2, nsmall = 2))
                  }
                }
              }
            }
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
              for(ch in for_chains){
                if(setparmfrow){par(mfrow = c(length(nonempty[[ch]]), length(Ys)))}
                for(g in nonempty[[ch]]){
                  for(y in Ys){
                    plot_ACF(mcmc = mcmc, what = what,
                             gspec = g, yspec = y, 
                             burnin = burnin, thin = thin, lag.max = lag.max,
                             iterations = iterations, chains = plot_chains[[ch]], 
                             labcex = ifelse(length(nonempty[[ch]])*length(Ys) > 1, 0.6, 1.0))
                  }
                }
              }
            }else{
              # not y-specific, but still g-specific
              for(ch in for_chains){
                if(setparmfrow){par(mfrow = nice_nrow_ncol(length(nonempty[[ch]])))}
                for(g in nonempty[[ch]]){
                  plot_ACF(mcmc = mcmc, what = what,
                           gspec = g, 
                           burnin = burnin, thin = thin, lag.max = lag.max,
                           iterations = iterations, chains = plot_chains[[ch]], 
                           labcex = ifelse(length(nonempty[[ch]]) > 1, 0.6, 1.0))
                }
              }
            }
          }
          
          if(v$D == 1){
            # one-dimensional parameter
            if(v$ydepd1){
              # the size of the dimension depends on outcomes
              for(ch in for_chains){
                for(y in Ys){
                  if(setparmfrow){par(mfrow = c(length(nonempty[[ch]]),mcmc$yspecd1[[what]][[y]]))}
                  for(g in nonempty[[ch]]){
                    for(i in 1:mcmc$yspecd1[[what]][[y]]){
                      plot_ACF(mcmc = mcmc, what = what,
                               gspec = g, yspec = y, dimspec = i, 
                               burnin = burnin, thin = thin, lag.max = lag.max,
                               iterations = iterations, chains = plot_chains[[ch]], labcex = 0.6)
                    }
                  }
                }
              }
            }else{
              # the size of the dimension depends on outcomes
              for(ch in for_chains){
                if(setparmfrow){par(mfrow = c(length(nonempty[[ch]]), v$d1))}
                for(g in nonempty[[ch]]){
                  for(i in 1:v$d1){
                    plot_ACF(mcmc = mcmc, what = what,
                             gspec = g, dimspec = i, 
                             burnin = burnin, thin = thin, lag.max = lag.max,
                             iterations = iterations, chains = plot_chains[[ch]], labcex = 0.6)
                  }
                }
              }
            }
          }
          
          if(v$D == 2){
            # two-dimensional parameter
            if((v$ydepd1) | (v$ydepd2)){
              # some size of the dimension depends on outcomes
              for(ch in for_chains){
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
                  for(g in nonempty[[ch]]){
                    if(setparmfrow){par(mfrow = c(d1,d2))}
                    for(i in 1:d1){
                      for(j in 1:d2){
                        plot_ACF(mcmc = mcmc, what = what,
                                 gspec = g, yspec = y, dimspec = c(i,j), 
                                 burnin = burnin, thin = thin, lag.max = lag.max,
                                 iterations = iterations, chains = plot_chains[[ch]], labcex = 0.6)
                      }
                    }
                  }
                }
              }
            }else{
              # none dimension is y-specific
              for(ch in for_chains){
                for(g in nonempty[[ch]]){
                  if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
                  for(i in 1:v$d1){
                    d2 <- ifelse(v$d2spec, mcmc$d2spec[[what]][i], v$d2)
                    for(j in 1:d2){
                      plot_ACF(mcmc = mcmc, what = what,
                               gspec = g, dimspec = c(i,j), 
                               burnin = burnin, thin = thin, lag.max = lag.max,
                               iterations = iterations, chains = plot_chains[[ch]], labcex = 0.6)
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
          }
          
        }else{
          # not group-specific parameter
          if(v$D == 0){
            # zero-dimensional parameter
            if(v$isy){
              # y-specific
              if(setparmfrow){par(mfrow = nice_nrow_ncol(length(Ys)))}
              for(y in Ys){
                plot_ACF(mcmc = mcmc, what = what,
                         yspec = y, 
                         burnin = burnin, thin = thin, lag.max = lag.max,
                         iterations = iterations, chains = chains, 
                         labcex = ifelse(length(Ys) > 1, 0.6, 1.0))
              }
            }else{
              # not y-specific, neither g-specific
              if(setparmfrow){par(mfrow = c(1,1))}
              plot_ACF(mcmc = mcmc, what = what, 
                       burnin = burnin, thin = thin, lag.max = lag.max,
                       iterations = iterations, chains = chains, 
                       labcex = 1.0)
            }
          }
          
          if(v$D == 1){
            # one-dimensional parameter
            if(v$ydepd1){
              # the size of the dimension depends on outcomes
              for(y in Ys){
                if(setparmfrow){par(mfrow = nice_nrow_ncol(mcmc$yspecd1[[what]][[y]]))}
                for(i in 1:mcmc$yspecd1[[what]][[y]]){
                  plot_ACF(mcmc = mcmc, what = what,
                           yspec = y, dimspec = i, 
                           burnin = burnin, thin = thin, lag.max = lag.max,
                           iterations = iterations, chains = chains, labcex = 0.6)
                }
              }
            }else{
              # the size of the dimension depends on outcomes
              if(setparmfrow){par(mfrow = nice_nrow_ncol(v$d1))}
              for(i in 1:v$d1){
                plot_ACF(mcmc = mcmc, what = what,
                         dimspec = i, 
                         burnin = burnin, thin = thin, lag.max = lag.max,
                         iterations = iterations, chains = chains, labcex = 0.6)
              }
            }
          }
          
          if(v$D == 2){
            # two-dimensional parameter
            if((v$ydepd1) | (v$ydepd2)){
              # some size of the dimension depends on outcomes
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
                    plot_ACF(mcmc = mcmc, what = what,
                             yspec = y, dimspec = c(i,j), 
                             burnin = burnin, thin = thin, lag.max = lag.max,
                             iterations = iterations, chains = chains, labcex = 0.6)
                  }
                }
              }
            }else{
              # none dimension is y-specific
              if(setparmfrow){par(mfrow = c(v$d1, v$d2))}
              for(i in 1:v$d1){
                d2 <- ifelse(v$d2spec, mcmc$d2spec[[what]][i], v$d2)
                for(j in 1:d2){
                  plot_ACF(mcmc = mcmc, what = what,
                           dimspec = c(i,j), 
                           burnin = burnin, thin = thin, lag.max = lag.max,
                           iterations = iterations, chains = chains, labcex = 0.6)
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
      }
    }else{
      warning("There is nothing to be plotted, parameter dimension is 0.")
    }
    
  }
