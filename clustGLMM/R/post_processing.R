post_processing <-
function(mcmc, cols, iter.max = 200, nstart = 30){
  if(!inherits(mcmc, "clustglmm")){
    stop("mcmc is not 'clustglmm' object")
  }
  
  
  ### 0) The type of saving results has to be "data.frame" for the code below
  backtolist <- FALSE
  if(mcmc$howsave=="list"){
    # Then transfer to howsave="data.frame" -- create mcmc$draws data.frame
    backtolist <- TRUE
    mcmc <- from_list_to_matrix(mcmc)
  } else if (mcmc$howsave != "data.frame") {
      stop("Not implemented for howsave = ", sQuote(mcmc$howsave),
           " yet.")
  }
  
  ### 1) Find the posterior draws with Gplus = modeGplus
  inds <- ms <- list()
  iters <- orig_iters <- numeric(mcmc$nchains)
  for(ch in mcmc$chains){
    submcmc <- mcmc$draws[[ch]][, c("m", "Gplus")]
    orig_iters[ch] <- nrow(submcmc)
    ms[[ch]] <- submcmc[submcmc$Gplus == mcmc$modeGplus[ch], "m"]
    inds[[ch]] <- which(submcmc$Gplus == mcmc$modeGplus[ch])
    iters[ch] <- length(inds[[ch]])
  }
  # message("modeGplus:")
  # print(mcmc$modeGplus)
  # message("Frequency of the modeGplus value:")
  # print(iters)
  
  ### 2) Columns of mcmc$draws that are cluster-specific parameters
  allgcols <- kmcols <- list()
  for(ch in mcmc$chains){
    allgcols[[ch]] <- colnames(mcmc$draws[[ch]])[grep("\\(\\d+\\)", colnames(mcmc$draws[[ch]]))]
    # but not clustering related variables (weights, sizes, probabilities)
    allgcols[[ch]] <- allgcols[[ch]][!(grepl("^w", allgcols[[ch]]) | 
                                         grepl("^ng", allgcols[[ch]]) | 
                                         grepl("^pUig", allgcols[[ch]]) | 
                                         grepl("^pUig_int", allgcols[[ch]]))]
    
    if(missing(cols)){
      kmcols[[ch]] <- allgcols[[ch]]
    }else{
      kmcols[[ch]] <- cols
    }
  }
  
  ### 3) Creating submatrices of sampled cluster-specific parameters
  clusterspec <- whichG <- list()
  for(ch in mcmc$chains){
    suball <- mcmc$draws[[ch]][inds[[ch]], 
                               c("m", "Gplus", paste0("ng(", 1:mcmc$G[ch], ")"), kmcols[[ch]])]
    clusterspec[[ch]] <- matrix(NA, nrow = iters[ch], ncol = 1 + length(kmcols[[ch]])/mcmc$G[ch]*mcmc$modeGplus[ch])
    whichG[[ch]] <- matrix(NA, nrow = iters[ch], ncol = mcmc$modeGplus[ch])
    for(m in 1:iters[ch]){
      whichG[[ch]][m,] <- which(suball[m, paste0("ng(", 1:mcmc$G[ch], ")")] > 0)
      #print(whichG)
      expr <- paste(paste0("\\(", whichG[[ch]][m,], "\\)"), collapse = "|")
      subcols <- kmcols[[ch]][grep(expr, kmcols[[ch]])]
      clusterspec[[ch]][m,] <- unlist(suball[m,c("m",subcols)])
    }
    expr <- paste(paste0("\\(", 1:mcmc$modeGplus[ch], "\\)"), collapse = "|")
    subcols <- kmcols[[ch]][grep(expr, kmcols[[ch]])]
    cnames <- c("m", subcols)
    colnames(clusterspec[[ch]]) <- cnames
  }
  # message("Subset of states of cluster-specific parameters with Gplus=modeGplus created for each chain separately.")
  
  
  ### 4) Prepare for k-means clustering
  forkmeans <- list()
  for(ch in mcmc$chains){
    forkmeans[[ch]] <- matrix(NA, nrow = 0, ncol = length(kmcols[[ch]])/mcmc$G[ch])
    for(g in 1:mcmc$modeGplus[ch]){
      forkmeans[[ch]] <- rbind(forkmeans[[ch]], clusterspec[[ch]][,grep(paste0("\\(", g, "\\)"), 
                                                                        colnames(clusterspec[[ch]])),drop=FALSE])
    }
    cnames <- colnames(clusterspec[[ch]])[grep("\\(1\\)", 
                                               colnames(clusterspec[[ch]]))]
    colnames(forkmeans[[ch]]) <- gsub("\\(1\\)", "", cnames)
  }
  # message("Group-specific parameters stacked below each other, prepared for k-means.")
  
  # message("Columns used for k-means:")
  # for(ch in mcmc$chains){
  #   print(colnames(forkmeans[[ch]]))
  # }
  
  ### 5) Perform K-means clustering + checking the permutations
  Mrhos <- numeric(mcmc$nchains)
  indmat <- rowuniques <- list()
  for(ch in mcmc$chains){
    # standardization
    forkmeans[[ch]] <- matrix(as.numeric(forkmeans[[ch]]), ncol = dim(forkmeans[[ch]])[2])
    colnames(forkmeans[[ch]]) <- paste0("V", 1:dim(forkmeans[[ch]])[2])
    #class(forkmeans[[ch]])
    #dim(forkmeans[[ch]])
    #summary(forkmeans[[ch]])
    forkmeans[[ch]] <- scale(as.matrix(forkmeans[[ch]]))
    km <- kmeans(forkmeans[[ch]], centers = mcmc$modeGplus[ch],
                 iter.max = iter.max, nstart = nstart)
    # create index matrix
    indmat[[ch]] <- matrix(km$cluster, ncol = mcmc$modeGplus[ch], byrow = FALSE)
    #head(indmat[[ch]])
    # number of not permutations = number of rows where is something repeated
    rowuniques[[ch]] <- apply(indmat[[ch]], 1, function(x){length(unique(x))})
    Mrhos[ch] <- sum(rowuniques[[ch]] < mcmc$modeGplus[ch])
  }
  # mcmc$Mrhos <- Mrhos
  # message("K-means performed")
  # message("Percentage of non-permutations within K-means")
  # print(paste0(Mrhos / iters * 100, " %"))
  
  ### 6) Update the clusterspec matrix according to suggested permutations
  ispermutation <- list()
  for(ch in mcmc$chains){
    ispermutation[[ch]] <- integer(iters[ch])
    #head(clusterspec[[ch]])
    cnames <- colnames(clusterspec[[ch]])[grep("\\(1\\)", 
                                               colnames(clusterspec[[ch]]))]
    for(m in 1:iters[ch]){
      ispermutation[[ch]][m] <- (rowuniques[[ch]][m] == mcmc$modeGplus[ch])
      if(ispermutation[[ch]][m]){
        # it is a permutation --> permute accordingly
        auxrow <- unlist(clusterspec[[ch]][m,])
        for(g in 1:mcmc$modeGplus[ch]){
          gnames <- gsub("\\(1\\)", paste0("\\(",g,"\\)"), cnames)
          pnames <- gsub("\\(1\\)", paste0("\\(",indmat[[ch]][m,g],"\\)"), cnames)
          clusterspec[[ch]][m,pnames] <- auxrow[gnames]
        }
      }else{
        # not a permutation --> replace with NA
        clusterspec[[ch]][m,] <- NA
      }
    }
  }
  # message("Each row permuted according to the found permutation")
  
  ### 7) New draws
  draws <- list()
  for(ch in mcmc$chains){
    # Parameters that are not group-specific and do not help with clustering
    nongcols <- setdiff(colnames(mcmc$draws[[ch]]), allgcols[[ch]])
    nongcols <- nongcols[!(grepl("^w", nongcols) | 
                             grepl("^ng",nongcols) | 
                             grepl("^pUig", nongcols)| 
                             grepl("^pUig_int", nongcols)|
                             grepl("^U", nongcols))]
    draws[[ch]] <- mcmc$draws[[ch]][inds[[ch]], nongcols]
    
    # Add permuted clusterspec - only cols
    draws[[ch]] <- cbind(draws[[ch]], clusterspec[[ch]])
    
    # Add other group-specific columns than those given
    othergcols <- setdiff(allgcols[[ch]], kmcols[[ch]])
    if(length(othergcols) > 0){
      suball <- mcmc$draws[[ch]][inds[[ch]], othergcols]
      cnames <- othergcols[grep("\\(1\\)", othergcols)]
      oGplus <- matrix(NA, nrow = iters[ch], ncol = mcmc$modeGplus[ch]*length(cnames))
      allcnames <- c()
      for(g in 1:mcmc$modeGplus[ch]){
        allcnames <- c(allcnames, gsub("\\(1\\)", paste0("\\(",g,"\\)"), cnames))
      }
      colnames(oGplus) <- allcnames
      
      for(m in 1:iters[ch]){
        if(ispermutation[[ch]][m]){
          expr <- paste(paste0("\\(", whichG[[ch]][m,], "\\)"), collapse = "|")
          subcols <- othergcols[grep(expr, othergcols)]
          auxrow <- unlist(suball[m,subcols,drop=FALSE])
          for(g in 1:mcmc$modeGplus[ch]){
            gnames <- gsub("\\(1\\)", paste0("\\(",whichG[[ch]][m,g],"\\)"), cnames)
            pnames <- gsub("\\(1\\)", paste0("\\(",indmat[[ch]][m,g],"\\)"), cnames)
            oGplus[m,pnames] <- auxrow[gnames]
          }
        }else{
          # remains NA
        }
      }
      draws[[ch]] <- cbind(draws[[ch]], oGplus)
    }
    
    # Add w and ng
    if(is.element("w", rownames(mcmc$settings[[ch]][mcmc$settings[[ch]]$save,]))){
      # if "ng" would not be saved, then it would already complain above
      suball <- mcmc$draws[[ch]][inds[[ch]], c(paste0("w(", 1:mcmc$G[ch], ")"),
                                               paste0("ng(", 1:mcmc$G[ch], ")"))]
      for(p in c("w", "ng")){
        pGplus <- matrix(NA, nrow = iters[ch], ncol = mcmc$modeGplus[ch])
        colnames(pGplus) <- paste0(p,"(", 1:mcmc$modeGplus[ch], ")")
        for(m in 1:iters[ch]){
          if(ispermutation[[ch]][m]){
            pcols <- paste0(p,"(", whichG[[ch]][m,], ")")
            auxrow <- unlist(suball[m,pcols,drop=FALSE])
            for(g in 1:mcmc$modeGplus[ch]){
              gname <- paste0(p,"(",whichG[[ch]][m,g],")")
              pname <- paste0(p,"(",indmat[[ch]][m,g],")")
              pGplus[m,pname] <- auxrow[gname]
            }
          }else{
            # remains NA
          }
        }
        draws[[ch]] <- cbind(draws[[ch]], pGplus)
      }
    } # end of adding w and ng
    
    # Add U
    if(is.element("U", rownames(mcmc$settings[[ch]][mcmc$settings[[ch]]$save,]))){
      suball <- mcmc$draws[[ch]][inds[[ch]], paste0("U[", 1:mcmc$n, "]")]
      UGplus <- matrix(NA, nrow = iters[ch], ncol = mcmc$n)
      colnames(UGplus) <- paste0("U[", 1:mcmc$n, "]")
      for(m in 1:iters[ch]){
        if(ispermutation[[ch]][m]){
          for(i in 1:mcmc$n){
            u <- suball[m,paste0("U[",i,"]")] 
            whu <- which(whichG[[ch]][m,] == u) # order within non-zero clusters
            UGplus[m,i] <- indmat[[ch]][m,whu] # applying permutation
          }
        }else{
          # remain NA
        }
      }
      draws[[ch]] <- cbind(draws[[ch]], UGplus)
    } # end of adding U
    
    # Add pUig, pUig_int
    for(p in c("pUig", "pUig_int")){
      if(is.element(p, rownames(mcmc$settings[[ch]][mcmc$settings[[ch]]$save,]))){
        pGplus <- matrix(NA, nrow = iters[ch], ncol = 0)
        for(i in 1:mcmc$n){
          suball <- mcmc$draws[[ch]][inds[[ch]], 
                                     c(paste0(p,"(",1:mcmc$G[ch],")[",i,"]"),
                                       paste0("ng(",1:mcmc$G[ch],")"))]
          pU <- matrix(NA, nrow = iters[ch], ncol = mcmc$modeGplus[ch])
          colnames(pU) <- paste0(p,"(",1:mcmc$modeGplus[ch],")[",i,"]")
          for(m in 1:iters[ch]){
            if(ispermutation[[ch]][m]){
              pcols <- paste0(p,"(",whichG[[ch]][m,],")[",i,"]")
              auxrow <- unlist(suball[m,pcols,drop=FALSE])
              for(g in 1:mcmc$modeGplus[ch]){
                gname <- paste0(p,"(",whichG[[ch]][m,g],")[",i,"]")
                pname <- paste0(p,"(",indmat[[ch]][m,g],")[",i,"]")
                pU[m,pname] <- auxrow[gname]
              }
            }else{
              # remain NA
            }
          }
          pGplus <- cbind(pGplus, pU)
        }
        draws[[ch]] <- cbind(draws[[ch]], pGplus)
      }
    } # end adding pUig, pUig_int
  } # end of creating draws
  
  ### 8) make the output of the same class as input
  # + make necessary changes
  clusters <- iterations <- list()
  clustering <- matrix(NA, nrow = as.numeric(mcmc$n), ncol = mcmc$nchains)
  certainty <- matrix(NA, nrow = as.numeric(mcmc$n), ncol = mcmc$nchains)
  for(ch in mcmc$chains){
    # iterations
    iterations[[ch]] <- draws[[ch]][ispermutation[[ch]]==1, "m"]
    # clusters
    clusters[[ch]] <- 1:mcmc$modeGplus[ch]
    # U samples
    if(mcmc$settings[[ch]]["U","save"]){
      Us <- draws[[ch]][,paste0("U[",1:mcmc$n,"]")]
      ccU <- apply(Us, 2, function(U, ch){
        tab <- table(factor(U, as.character(clusters[[ch]])))
        c(as.numeric(names(which.max(tab))), max(tab, na.rm=TRUE)/iters[ch])
      }, ch=ch)
      clustering[,ch] <- ccU[1, ]
      certainty[,ch] <- ccU[2, ]
    }else{
      # we no longer have allocation indicators saved
      # remains as NA
      # TODO think about how it could be done? ... 
      #      problem is each m could be permuted differently
    }
  }
  rownames(clustering) <- rownames(certainty) <- names(mcmc$numbered_unique_ids)

  sameclusters <- TRUE
  for(ch in mcmc$chains[-1]){
    sameclusters <- (sameclusters & setequal(clusters[[mcmc$chains[1]]], clusters[[ch]]))
  }
  mcmc$sameclusters <- sameclusters
  
  ### Final output
  res <- mcmc
  res$iterations <- iterations 
  res$clusters <- clusters
  res$clustering <- clustering
  res$certainty <- certainty
  res$sameclusters <- sameclusters
  res$draws <- draws
  res$G <- mcmc$modeGplus
  
  lsettings <- list()
  for(ch in mcmc$chains){
    s <- mcmc$settings[[ch]]
    s$iter <- iters[ch]
    s$G <- mcmc$modeGplus[ch]
    s$dimswithG <- s$dims * ((!s$gspec)*1 + (s$gspec)*mcmc$modeGplus[ch])
    lsettings[[ch]] <- s
  }
  res$settings <- lsettings
  
  param_names <- list()
  for(ch in mcmc$chains){
    param_names[[ch]] <- list()
    for(p in rownames(lsettings[[ch]])){
      if(lsettings[[ch]][p,"save"] & (lsettings[[ch]][p,"dims"] > 0)){
        aux = from_C_to_matrix(values = rep(0, lsettings[[ch]][p,"dimswithG"]),
                               p = p,
                               settings = lsettings[[ch]],
                               yspecd1 = mcmc$yspecd1[[p]],
                               yspecd2 = mcmc$yspecd2[[p]],
                               family = mcmc$family)
        param_names[[ch]][[p]] <- colnames(aux)
      }else{
        param_names[[ch]][[p]] <- c()
      }
    }
  }
  res$param_names <- param_names
  
  # TODO
  # inits, last? 
  # iter? the same --> will be always the maximum value
  # res$post_processed <- TRUE
  last <- list()
  for(ch in mcmc$chains){
    last[[ch]] <- list()
    m <- iters[ch]
    while(m > 0 && (rowuniques[[ch]][m] != mcmc$modeGplus[ch])){
      m <- m-1
    }
    if(m == 0){
      # there is no iteration satisfying that condition --> do not change anything
    }else{
      # m is the last iteration with modeGplus filled components that resulted in a permutation
      values <- unlist(draws[[ch]][m, ])
      # w
      if(lsettings[[ch]]["w","save"]){
        wvalues <- values[paste0("w(",1:mcmc$modeGplus[ch],")")]
      }else{
        wvalues <- table(clustering[,ch])
      }
      last[[ch]]$w <- wvalues / sum(wvalues)
      # e0
      if(lsettings[[ch]]["e0","save"]){
        e0values <- values["e0"]
      }else{
        e0values <- mcmc$param$e0_shp / mcmc$param$e0_rte
      }
      last[[ch]]$e0 <- e0values
      # U
      if(lsettings[[ch]]["U","save"]){
        Uvalues <- values[paste0("U[",1:mcmc$n,"]")]
      }else{
        Uvalues <- clustering[,ch]
      }
      last[[ch]]$U <- Uvalues
      # pUig
      if(lsettings[[ch]]["pUig","save"]){
        pvalues <- values[param_names[[ch]]$pUig]
      }else{
        pvalues <- rep(last[[ch]]$w, each=mcmc$n)
      }
      last[[ch]]$pUig <- matrix(pvalues, mcmc$n, mcmc$modeGplus[ch], byrow = FALSE)
      
      # prec_num
      last[[ch]]$prec_num <- list()
      if(lsettings[[ch]]["prec_num","save"]){
        if(mcmc$varying["prec_num"]){
          for(y in mcmc$Nums){
            last[[ch]]$prec_num[[y]] <- list()
          }
          for(g in 1:mcmc$modeGplus[ch]){
            for(y in mcmc$Nums){
              last[[ch]]$prec_num[[y]][[g]] <- values[paste0("prec_num_",y,"(",g,")")]
            }
          }
        }else{
          for(y in mcmc$Nums){
            last[[ch]]$prec_num[[y]] <- values[paste0("prec_num_",y)]
          }
        }
      }else{
        if(mcmc$varying["prec_num"]){
          for(y in mcmc$Nums){
            last[[ch]]$prec_num[[y]] <- list()
          }
          for(g in 1:mcmc$modeGplus[ch]){
            for(y in mcmc$Nums){
              last[[ch]]$prec_num[[y]][[indmat[[ch]][m,g]]] <- 
                mcmc$last[[ch]]$prec_num[[y]][[whichG[[ch]][m,g]]]
            }
          }
        }else{
          last[[ch]]$prec_num <- mcmc$last[[ch]]$prec_num
        }
      }
      
      # beta_num_fix
      last[[ch]]$beta_num_fix <- list()
      if(lsettings[[ch]]["beta_num_fix","save"]){
        for(y in mcmc$Nums){
          if(mcmc$nfix[y]>0){
            last[[ch]]$beta_num_fix[[y]] <- values[paste0("beta_num_fix_",y,"[",1:mcmc$nfix[y],"]")]
          }else{
            last[[ch]]$beta_num_fix[[y]] <- numeric()
          }
        }
      }else{
        last[[ch]]$beta_num_fix <- mcmc$last[[ch]]$beta_num_fix
      }
      
      # beta_num
      last[[ch]]$beta_num <- list()
      for(y in mcmc$Nums){
        last[[ch]]$beta_num[[y]] <- list()
      }
      if(lsettings[[ch]]["beta_num","save"]){
        for(g in 1:mcmc$modeGplus[ch]){
          for(y in mcmc$Nums){
            if(mcmc$ngrp[y]>0){
              last[[ch]]$beta_num[[y]][[g]] <- values[paste0("beta_num_",y,"(",g,")[",1:mcmc$ngrp[y],"]")]
            }else{
              last[[ch]]$beta_num[[y]][[g]] <- numeric()
            }
          }
        }
      }else{
        for(y in mcmc$Nums){
          for(g in 1:mcmc$modeGplus[ch]){
            last[[ch]]$beta_num[[y]][[indmat[[ch]][m,g]]] <- 
              mcmc$last[[ch]]$beta_num[[y]][[whichG[[ch]][m,g]]]
          }
        }
      }
      
      # beta_poi_fix
      last[[ch]]$beta_poi_fix <- list()
      if(lsettings[[ch]]["beta_poi_fix","save"]){
        for(y in mcmc$Pois){
          if(mcmc$nfix[y]>0){
            last[[ch]]$beta_poi_fix[[y]] <- values[paste0("beta_poi_fix_",y,"[",1:mcmc$nfix[y],"]")]
          }else{
            last[[ch]]$beta_poi_fix[[y]] <- numeric()
          }
        }
      }else{
        last[[ch]]$beta_poi_fix <- mcmc$last[[ch]]$beta_poi_fix
      }
      
      # beta_poi
      last[[ch]]$beta_poi <- list()
      for(y in mcmc$Pois){
        last[[ch]]$beta_poi[[y]] <- list()
      }
      if(lsettings[[ch]]["beta_poi","save"]){
        for(g in 1:mcmc$modeGplus[ch]){
          for(y in mcmc$Pois){
            if(mcmc$ngrp[y]>0){
              last[[ch]]$beta_poi[[y]][[g]] <- values[paste0("beta_poi_",y,"(",g,")[",1:mcmc$ngrp[y],"]")]
            }else{
              last[[ch]]$beta_poi[[y]][[g]] <- numeric()
            }
          }
        }
      }else{
        for(y in mcmc$Pois){
          for(g in 1:mcmc$modeGplus[ch]){
            last[[ch]]$beta_poi[[y]][[indmat[[ch]][m,g]]] <- 
              mcmc$last[[ch]]$beta_poi[[y]][[whichG[[ch]][m,g]]]
          }
        }
      }
      
      # beta_bin_fix
      last[[ch]]$beta_bin_fix <- list()
      if(lsettings[[ch]]["beta_bin_fix","save"]){
        for(y in mcmc$Bins){
          if(mcmc$nfix[y]>0){
            last[[ch]]$beta_bin_fix[[y]] <- values[paste0("beta_bin_fix_",y,"[",1:mcmc$nfix[y],"]")]
          }else{
            last[[ch]]$beta_bin_fix[[y]] <- numeric()
          }
        }
      }else{
        last[[ch]]$beta_bin_fix <- mcmc$last[[ch]]$beta_bin_fix
      }
      
      # beta_bin
      last[[ch]]$beta_bin <- list()
      for(y in mcmc$Bins){
        last[[ch]]$beta_bin[[y]] <- list()
      }
      if(lsettings[[ch]]["beta_bin","save"]){
        for(g in 1:mcmc$modeGplus[ch]){
          for(y in mcmc$Bins){
            if(mcmc$ngrp[y]>0){
              last[[ch]]$beta_bin[[y]][[g]] <- values[paste0("beta_bin_",y,"(",g,")[",1:mcmc$ngrp[y],"]")]
            }else{
              last[[ch]]$beta_bin[[y]][[g]] <- numeric()
            }
          }
        }
      }else{
        for(y in mcmc$Bins){
          for(g in 1:mcmc$modeGplus[ch]){
            last[[ch]]$beta_bin[[y]][[indmat[[ch]][m,g]]] <- 
              mcmc$last[[ch]]$beta_bin[[y]][[whichG[[ch]][m,g]]]
          }
        }
      }
      
      # beta_ord_fix
      last[[ch]]$beta_ord_fix <- list()
      if(lsettings[[ch]]["beta_ord_fix","save"]){
        for(y in mcmc$Ords){
          if(mcmc$nfix[y]>0){
            last[[ch]]$beta_ord_fix[[y]] <- values[paste0("beta_ord_fix_",y,"[",1:mcmc$nfix[y],"]")]
          }else{
            last[[ch]]$beta_ord_fix[[y]] <- numeric()
          }
        }
      }else{
        last[[ch]]$beta_ord_fix <- mcmc$last[[ch]]$beta_ord_fix
      }
      
      # beta_ord
      last[[ch]]$beta_ord <- list()
      for(y in mcmc$Ords){
        last[[ch]]$beta_ord[[y]] <- list()
      }
      if(lsettings[[ch]]["beta_ord","save"]){
        for(g in 1:mcmc$modeGplus[ch]){
          for(y in mcmc$Ords){
            if(mcmc$ngrp[y]>0){
              last[[ch]]$beta_ord[[y]][[g]] <- values[paste0("beta_ord_",y,"(",g,")[",1:mcmc$ngrp[y],"]")]
            }else{
              last[[ch]]$beta_ord[[y]][[g]] <- numeric()
            }
          }
        }
      }else{
        for(y in mcmc$Ords){
          for(g in 1:mcmc$modeGplus[ch]){
            last[[ch]]$beta_ord[[y]][[indmat[[ch]][m,g]]] <- 
              mcmc$last[[ch]]$beta_ord[[y]][[whichG[[ch]][m,g]]]
          }
        }
      }
      
      # c_ord, a_ord, pi_ord
      for(p in c("c_ord", "a_ord", "pi_ord")){
        last[[ch]][[p]] <- list()
        if(lsettings[[ch]][p,"save"]){
          if(mcmc$varying["c_ord"]){
            for(y in mcmc$Ords){
              last[[ch]][[p]][[y]] <- list()
            }
            for(g in 1:mcmc$modeGplus[ch]){
              for(y in mcmc$Ords){
                last[[ch]][[p]][[y]][[g]] <- values[paste0(p,"_",y,"(",g,")[",
                                                           1:(mcmc$Kord[y]+1*(p=="pi_ord")),
                                                           "]")]
              }
            }
          }else{
            for(y in mcmc$Ords){
              last[[ch]][[p]][[y]] <- values[paste0(p,"_",y,"[",
                                                    1:(mcmc$Kord[y]+1*(p=="pi_ord")),
                                                    "]")]
            }
          }
        }else{
          if(mcmc$varying["c_ord"]){
            for(y in mcmc$Ords){
              last[[ch]][[p]][[y]] <- list()
            }
            for(g in 1:mcmc$modeGplus[ch]){
              for(y in mcmc$Ords){
                last[[ch]][[p]][[y]][[indmat[[ch]][m,g]]] <- 
                  mcmc$last[[ch]][[p]][[y]][[whichG[[ch]][m,g]]]
              }
            }
          }else{
            last[[ch]][[p]] <- mcmc$last[[ch]][[p]]
          }
        }
      }
      
      # beta_cat_fix
      last[[ch]]$beta_cat_fix <- list()
      if(lsettings[[ch]]["beta_cat_fix","save"]){
        for(y in mcmc$Cats){
          if(mcmc$nfix[y]>0){
            last[[ch]]$beta_cat_fix[[y]] <- matrix(values[paste0("beta_cat_fix_",y,
                                                                 "[",
                                                                 rep(1:mcmc$nfix[y], each=mcmc$Kcat[y]),
                                                                 ",",
                                                                 rep(1:mcmc$Kcat[y], mcmc$nfix[y]),
                                                                 "]")],
                                                   nrow = mcmc$nfix[y], byrow=TRUE)
          }else{
            last[[ch]]$beta_cat_fix[[y]] <- numeric()
          }
        }
      }else{
        last[[ch]]$beta_cat_fix <- mcmc$last[[ch]]$beta_cat_fix
      }
      
      # beta_cat
      last[[ch]]$beta_cat <- list()
      for(y in mcmc$Cats){
        last[[ch]]$beta_cat[[y]] <- list()
      }
      if(lsettings[[ch]]["beta_cat","save"]){
        for(g in 1:mcmc$modeGplus[ch]){
          for(y in mcmc$Cats){
            if(mcmc$ngrp[y]>0){
              last[[ch]]$beta_cat[[y]][[g]] <- matrix(values[paste0("beta_cat_",y,
                                                                    "(",g,")[",
                                                                    rep(1:mcmc$ngrp[y], each=mcmc$Kcat[y]),
                                                                    ",",
                                                                    rep(1:mcmc$Kcat[y], mcmc$ngrp[y]),
                                                                    "]")],
                                                      nrow = mcmc$ngrp[y], byrow=TRUE)
            }else{
              last[[ch]]$beta_cat[[y]][[g]] <- numeric()
            }
          }
        }
      }else{
        for(y in mcmc$Cats){
          for(g in 1:mcmc$modeGplus[ch]){
            last[[ch]]$beta_cat[[y]][[indmat[[ch]][m,g]]] <- 
              mcmc$last[[ch]]$beta_cat[[y]][[whichG[[ch]][m,g]]]
          }
        }
      }
      
      # random-effects related parameters (only if random effects are present)
      if(mcmc$totnran > 0){
        # InvSigma, InvQ
        for(p in c("InvSigma", "InvQ")){
          if(lsettings[[ch]][p,"save"]){
            if(mcmc$varying[p]){
              last[[ch]][[p]] <- list()
              for(g in 1:mcmc$modeGplus[ch]){
                pommatrix <- matrix(0, mcmc$totnran, mcmc$totnran)
                pomvec <- values[grepl(paste0("^",p,"\\(",g,"\\)"),names(values))]
                pommatrix[upper.tri(pommatrix, diag = TRUE)] <- pomvec
                pommatrix <- t(pommatrix)
                pommatrix[upper.tri(pommatrix, diag = TRUE)] <- pomvec
                last[[ch]][[p]][[g]] <- pommatrix
              }
            }else{
              pommatrix <- matrix(0, mcmc$totnran, mcmc$totnran)
              pomvec <- values[grepl(paste0("^",p),names(values))]
              pommatrix[upper.tri(pommatrix, diag = TRUE)] <- pomvec
              pommatrix <- t(pommatrix)
              pommatrix[upper.tri(pommatrix, diag = TRUE)] <- pomvec
              last[[ch]][[p]] <- pommatrix
            }
          }else{
            if(mcmc$varying[p]){
              for(g in 1:mcmc$modeGplus[ch]){
                last[[ch]][[p]][[indmat[[ch]][m,g]]] <- 
                  mcmc$last[[ch]][[p]][[whichG[[ch]][m,g]]]
              }
            }else{
              last[[ch]][[p]] <- mcmc$last[[ch]][[p]]
            }
          }
        }
        # b
        if(lsettings[[ch]][p,"save"]){
          last[[ch]]$b <- matrix(values[paste0("b[",
                                               rep(1:mcmc$n, each=mcmc$totnran),
                                               ",",
                                               rep(1:mcmc$totnran, mcmc$n),
                                               "]")], 
                                 mcmc$n, mcmc$totnran, byrow=TRUE)
        }else{
          last[[ch]]$b <-mcmc$last[[ch]]$b
        }
      }
      
      # naY
      if(lsettings[[ch]]["naY","dims"]>0){
        if(lsettings[[ch]]["naY","save"]){
          last[[ch]]$naY <- list()
          if(mcmc$varying["naY"]){
            for(g in 1:mcmc$modeGplus[ch]){
              last[[ch]]$naY[[g]] <- list()
              for(y in mcmc$Ys){
                if(mcmc$yspecd1$naY[y] > 0){
                  last[[ch]]$naY[[g]][[y]] <- values[paste0("naY_",y,"(",g,")[",1:mcmc$yspecd1$naY[y],"]")]
                }else{
                  last[[ch]]$naY[[g]][[y]] <- numeric()
                }
              }
            }
          }else{
            for(y in mcmc$Ys){
              if(mcmc$yspecd1$naY[y] > 0){
                last[[ch]]$naY[[y]] <- values[paste0("naY_",y,"[",1:mcmc$yspecd1$naY[y],"]")]
              }else{
                last[[ch]]$naY[[y]] <- numeric()
              }
            }
          }
        }else{
          if(mcmc$varying["naY"]){
            for(g in 1:mcmc$modeGplus[ch]){
              last[[ch]]$naY[[indmat[[ch]][m,g]]] <- 
                mcmc$last[[ch]]$naY[[whichG[[ch]][m,g]]]
            }
          }else{
            last[[ch]]$naY <- mcmc$last[[ch]]$naY
          }
        }
      }
    } # end of else m==0
  } # end for chain in mcmc$chains
  res$last <- last
  
  # call
  call <- c("###------------------------------------------###\n")
  call <- paste0(call, "### Post-processed MCMC samples of clustGLMM ###\n")
  call <- paste0(call, "###------------------------------------------###\n")
  for(ch in mcmc$chains){
    call <- paste0(call, "-----------------------------------------------------------------------\n")
    call <- paste0(call, "Chain ", ch, "\n")
    call <- paste0(call, "-----------------------------------------------------------------------\n")
    call <- paste0(call, "The new number of components for this chain: $G[",ch,"] = ", res$G[ch], ".\n")
    
    sentence <- paste0("We now have components: ", 
                       paste0(res$clusters[[ch]], collapse = ", "), ".")
    wsent <- strwrap(sentence, width = getOption("width"))
    for(l in 1:length(wsent)){
      call <- paste0(call, wsent[l], "\n")
    }
    
    sentence <- paste0("Number of iterations with ", 
                       mcmc$modeGplus[ch], 
                       " components: ", 
                       iters[ch], 
                       " (", 
                       format(iters[ch] / orig_iters[ch] * 100, digits=2, nsmall = 2), 
                       " %)")
    wsent <- strwrap(sentence, width = getOption("width"))
    for(l in 1:length(wsent)){
      call <- paste0(call, wsent[l], "\n")
    }
    
    sentence <- paste0("Out of them, ", Mrhos[ch], " (", 
                       format(Mrhos[ch] / iters[ch] * 100, digits=2, nsmall = 2), 
                       " %) did not lead to a permutation when using k-means, these g-specific parameters are replaced with NA.")
    wsent <- strwrap(sentence, width = getOption("width"))
    for(l in 1:length(wsent)){
      call <- paste0(call, wsent[l], "\n")
    }
    
    sentence <- paste0("Therefore, ", iters[ch] - Mrhos[ch], " (",
                       format((iters[ch] - Mrhos[ch]) / iters[ch] * 100, digits=2, nsmall = 2), 
                       " %) iterations have been permuted and aligned. These are now eligible for the analysis.")
    wsent <- strwrap(sentence, width = getOption("width"))
    for(l in 1:length(wsent)){
      call <- paste0(call, wsent[l], "\n")
    }
    call <- paste0(call, "\n")
  }
  res$call <- call
  
  
  
  if(backtolist){
    res <- from_matrix_to_list(res)
  }
  class(res) <- "clustglmm"
  
  return(res)
}
