rescale_list <- function(draws, settings, 
                         family, lfixnames, lgrpnames, lrannames,
                         Kord, Kcat,
                         centers, scales){
  # outcomes by type
  Nums <- names(family)[is.element(family, c("num", "gaussian"))]
  Pois <- names(family)[is.element(family, c("poi", "poisson"))]
  Bins <- names(family)[is.element(family, c("bin", "bernoulli"))]
  Ords <- names(family)[is.element(family, c("ord", "cumulative"))]
  Cats <- names(family)[is.element(family, c("cat", "categorical"))]
  Ys <- c(Nums, Pois, Bins, Ords, Cats)
  
  ### beta_num, beta_num_fix, prec_num, sd_num, var_num
  # and naY
  for(y in Nums){
    int_fix <- which(lfixnames[[y]]=="(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for(x in notint_fix){
      draws$beta_num_fix[[y]][,x] <- draws$beta_num_fix[[y]][,x] * scales[y] / scales[lfixnames[[y]][x]]
    }
    
    int_grp <- which(lgrpnames[[y]]=="(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for(x in notint_grp){
      for(g in 1:settings["beta_num", "G"]){
        draws$beta_num[[g]][[y]][,x] <- draws$beta_num[[g]][[y]][,x] * scales[y] / scales[lgrpnames[[y]][x]]
      }
    }
    
    # precompute shift by beta_num_fix
    if(length(notint_fix) > 0){
      shift_fix <- as.matrix(draws$beta_num_fix[[y]][,notint_fix,drop=FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- data.frame(x = rep(0, length(draws$m))) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if(length(int_fix) > 0){
      # Intercept is among fixed effects
      draws$beta_num_fix[[y]][,int_fix] <- centers[y] + scales[y] * draws$beta_num_fix[[y]][,int_fix]
      if(length(notint_fix) > 0){
        draws$beta_num_fix[[y]][,int_fix] <- draws$beta_num_fix[[y]][,int_fix] - shift_fix
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if(length(int_grp) > 0){
      # Intercept is among group-specific effects
      for(g in 1:settings["beta_num", "G"]){
        draws$beta_num[[g]][[y]][,int_grp] <- centers[y] + scales[y] * draws$beta_num[[g]][[y]][,int_grp]
        # subtract scaled centers combined with beta_num_fix
        if(length(notint_fix) > 0){
          draws$beta_num[[g]][[y]][,int_grp] <- draws$beta_num[[g]][[y]][,int_grp] - shift_fix
        }
        # subtract scaled centers combined with group-specific beta_num
        if(length(notint_grp) > 0){
          draws$beta_num[[g]][[y]][,int_grp] <- draws$beta_num[[g]][[y]][,int_grp] - as.matrix(draws$beta_num[[g]][[y]][,notint_grp,drop=FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
        }
      }
    }
    
    # prec_num, sd_num, var_num
    for(p in c("prec_num", "sd_num", "var_num")){
      if(settings[p,"save"]){
        multiplier <- switch(p,
                             prec_num = 1/scales[y]^2,
                             sd_num = scales[y],
                             var_num = scales[y]^2)
        if(settings[p,"gspec"]){
          for(g in 1:settings[p,"G"]){
            draws[[p]][[g]][,y] <- draws[[p]][[g]][,y] * multiplier
          }
        }else{
          draws[[p]][,y] <- draws[[p]][,y] * multiplier
        }
      }
    }
    
    # naY
    if(settings["naY","save"]){
      if(settings["naY","gspec"]){
        for(g in 1:settings["naY","G"]){
          if (!is.null(draws$naY[[g]][[y]])) {
            draws$naY[[g]][[y]] <- draws$naY[[g]][[y]] * scales[y] + centers[y]
          }
        }
      }else{
        if (!is.null(draws$naY[[y]])) {
            draws$naY[[y]] <- draws$naY[[y]] * scales[y] + centers[y]
        }
      }
    }
  } # end of for y in Nums
  
  ### beta_poi, beta_poi_fix
  for(y in Pois){
    int_fix <- which(lfixnames[[y]]=="(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for(x in notint_fix){
      draws$beta_poi_fix[[y]][,x] <- draws$beta_poi_fix[[y]][,x] / scales[lfixnames[[y]][x]]
    }
    
    int_grp <- which(lgrpnames[[y]]=="(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for(x in notint_grp){
      for(g in 1:settings["beta_poi", "G"]){
        draws$beta_poi[[g]][[y]][,x] <- draws$beta_poi[[g]][[y]][,x] / scales[lgrpnames[[y]][x]]
      }
    }
    
    # precompute shift by beta_poi_fix
    if(length(notint_fix) > 0){
      shift_fix <- as.matrix(draws$beta_poi_fix[[y]][,notint_fix,drop=FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- data.frame(x = rep(0, length(draws$m))) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if(length(int_fix) > 0){
      # Intercept is among fixed effects
      if(length(notint_fix) > 0){
        draws$beta_poi_fix[[y]][,int_fix] <- draws$beta_poi_fix[[y]][,int_fix] - shift_fix
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if(length(int_grp) > 0){
      # Intercept is among group-specific effects
      for(g in 1:settings["beta_poi", "G"]){
        # subtract scaled centers combined with beta_poi_fix
        if(length(notint_fix) > 0){
          draws$beta_poi[[g]][[y]][,int_grp] <- draws$beta_poi[[g]][[y]][,int_grp] - shift_fix
        }
        # subtract scaled centers combined with group-specific beta_poi
        if(length(notint_grp) > 0){
          draws$beta_poi[[g]][[y]][,int_grp] <- draws$beta_poi[[g]][[y]][,int_grp] - as.matrix(draws$beta_poi[[g]][[y]][,notint_grp,drop=FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
        }
      }
    }
  } # end of for y in Pois
  
  
  ### beta_bin, beta_bin_fix
  for(y in Bins){
    int_fix <- which(lfixnames[[y]]=="(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for(x in notint_fix){
      draws$beta_bin_fix[[y]][,x] <- draws$beta_bin_fix[[y]][,x] / scales[lfixnames[[y]][x]]
    }
    
    int_grp <- which(lgrpnames[[y]]=="(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for(x in notint_grp){
      for(g in 1:settings["beta_bin", "G"]){
        draws$beta_bin[[g]][[y]][,x] <- draws$beta_bin[[g]][[y]][,x] / scales[lgrpnames[[y]][x]]
      }
    }
    
    # precompute shift by beta_bin_fix
    if(length(notint_fix) > 0){
      shift_fix <- as.matrix(draws$beta_bin_fix[[y]][,notint_fix,drop=FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- data.frame(x = rep(0, length(draws$m))) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if(length(int_fix) > 0){
      # Intercept is among fixed effects
      if(length(notint_fix) > 0){
        draws$beta_bin_fix[[y]][,int_fix] <- draws$beta_bin_fix[[y]][,int_fix] - shift_fix
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if(length(int_grp) > 0){
      # Intercept is among group-specific effects
      for(g in 1:settings["beta_bin", "G"]){
        # subtract scaled centers combined with beta_bin_fix
        if(length(notint_fix) > 0){
          draws$beta_bin[[g]][[y]][,int_grp] <- draws$beta_bin[[g]][[y]][,int_grp] - shift_fix
        }
        # subtract scaled centers combined with group-specific beta_bin
        if(length(notint_grp) > 0){
          draws$beta_bin[[g]][[y]][,int_grp] <- draws$beta_bin[[g]][[y]][,int_grp] - as.matrix(draws$beta_bin[[g]][[y]][,notint_grp,drop=FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
        }
      }
    }
  } # end of for y in Bins
  
  
  ### beta_ord, beta_ord_fix, c_ord, a_ord, pi_ord
  for(y in Ords){
    notint_fix <- seq_len(length(lfixnames[[y]])) # could be empty
    for(x in notint_fix){
      draws$beta_ord_fix[[y]][,x] <- draws$beta_ord_fix[[y]][,x] / scales[lfixnames[[y]][x]]
    }
    
    notint_grp <- seq_len(length(lgrpnames[[y]])) # could be empty
    for(x in notint_grp){
      for(g in 1:settings["beta_ord", "G"]){
        draws$beta_ord[[g]][[y]][,x] <- draws$beta_ord[[g]][[y]][,x] / scales[lgrpnames[[y]][x]]
      }
    }
    
    # precompute shift by beta_ord_fix
    if(length(notint_fix) > 0){
      shift_fix <- as.matrix(draws$beta_ord_fix[[y]][,notint_fix,drop=FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- matrix(0, nrow = length(draws$m), ncol = 1)
    }
    # For ordinal outcomes we have ordered intercepts c_ord
    if(settings["c_ord","gspec"]){
      for(g in 1:settings["c_ord", "G"]){
        shift <- shift_fix + as.matrix(draws$beta_ord[[g]][[y]][,notint_grp,drop=FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
        for(k in 1:Kord[y]){
          draws$c_ord[[g]][[y]][,k] <- draws$c_ord[[g]][[y]][,k] + shift
        }
        # logits
        logits <- matrix(0, nrow = length(draws$m), ncol = Kord[y] + 2)
        logits[,Kord[y]+2] <- 1
        for(k in 1:Kord[y]){
          logits[,k+1] <- plogis(draws$c_ord[[g]][[y]][,k])
        }
        # a_ord
        if(settings["a_ord", "save"]){
          for(k in 1:Kord[y]){
            draws$a_ord[[g]][[y]][,k] <- log((logits[,k+1] - logits[,k])/(1-logits[,Kord[y]+1]))
          }
        }
        # pi_ord
        if(settings["pi_ord", "save"]){
          for(k in 1:(Kord[y]+1)){
            draws$pi_ord[[g]][[y]][,k] <- logits[,k+1] - logits[,k]
          }
        }
      }
    }else{
      # There should be no group-specific effects --> error already before
      # or c_ord has been made group-specific
      for(k in 1:Kord[y]){
        draws$c_ord[[y]][,k] <- draws$c_ord[[y]][,k] + shift_fix
      }
      # logits
      logits <- matrix(0, nrow = length(draws$m), ncol = Kord[y] + 2)
      logits[,Kord[y]+2] <- 1
        for(k in 1:Kord[y]){
          logits[,k+1] <- plogis(draws$c_ord[[y]][,k])
      }
      # a_ord
      if(settings["a_ord", "save"]){
        for(k in 1:Kord[y]){
          draws$a_ord[[y]][,k] <- log((logits[,k+1] - logits[,k])/(1-logits[,Kord[y]+1]))
        }
      }
      # pi_ord
      if(settings["pi_ord", "save"]){
        for(k in 1:(Kord[y]+1)){
          draws$pi_ord[[y]][,k] <- logits[,k+1] - logits[,k]
        }
      }
    } # end of if/else c_ord group-specific
  } # end of for y in Ords
  
  ### beta_cat, beta_cat_fix
  for(y in Cats){
    int_fix <- which(lfixnames[[y]]=="(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for(x in notint_fix){
      for(k in 1:Kcat[y]){
        draws$beta_cat_fix[[y]][,x,k] <- draws$beta_cat_fix[[y]][,x,k] / scales[lfixnames[[y]][x]]
      }
    }
    
    int_grp <- which(lgrpnames[[y]]=="(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for(x in notint_grp){
      for(g in 1:settings["beta_cat", "G"]){
        for(k in 1:Kcat[y]){
          draws$beta_cat[[g]][[y]][,x,k] <- draws$beta_cat[[g]][[y]][,x,k] / scales[lgrpnames[[y]][x]]
        }
      }
    }
    
    # precompute shift by beta_cat_fix
    shift_fix <- data.frame(matrix(0, nrow=length(draws$m), ncol=Kcat[y]))
    if(length(notint_fix) > 0){
      for(k in 1:Kcat[y]){
        shift_fix[,k] <- as.matrix(draws$beta_cat_fix[[y]][,notint_fix,k,drop=FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
      }
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if(length(int_fix) > 0){
      # Intercept is among fixed effects
      for(k in 1:Kcat[y]){
        if(length(notint_fix) > 0){
          draws$beta_cat_fix[[y]][,int_fix,k] <- draws[,int_fix,k] - shift_fix[,k]
        }
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if(length(int_grp) > 0){
      # Intercept is among group-specific effects
      for(g in 1:settings["beta_cat", "G"]){
        for(k in 1:Kcat[y]){
          # subtract scaled centers combined with beta_cat_fix
          if(length(notint_fix) > 0){
            draws$beta_cat[[g]][[y]][,int_grp,k] <- draws$beta_cat[[g]][[y]][,int_grp,k] - shift_fix[,k]
          }
          # subtract scaled centers combined with group-specific beta_cat
          if(length(notint_grp) > 0){
            draws$beta_cat[[g]][[y]][,int_grp,k] <- draws$beta_cat[[g]][[y]][,int_grp,k] - as.matrix(draws$beta_cat[[g]][[y]][,notint_grp,k,drop=FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
          }
        }
      }
    }
  } # end of for y in Cats
  
  
  ### Random effects b
  totnran <- sum(unlist(lapply(lrannames, length)))
  if(totnran > 0){
    if(settings["b","save"]){
      yadd <- 0
      for(y in Ys){
        yscl <- ifelse(is.element(y,Nums), scales[y], 1.0)
        int_ran <- which(lrannames[[y]]=="(Intercept)") # could be empty
        notint_ran <- setdiff(seq_len(length(lrannames[[y]])), int_ran) # could be empty
        # First, update random effects that are not intercepts
        for(z in notint_ran){
          draws$b[,,z+yadd] <- draws$b[,,z+yadd] * yscl / scales[lrannames[[y]][z]]
        }
        # Now update random intercepts (can be empty only if notint_ran is empty)
        if(length(int_ran) > 0){
          for(i in 1:settings["b","d1"]){
            draws$b[,i,int_ran+yadd] <- yscl * draws$b[,i,int_ran+yadd] 
            if(length(notint_ran) > 0){
              draws$b[,i,int_ran+yadd] <- draws$b[,i,int_ran+yadd] - as.matrix(draws$b[,i,notint_ran+yadd,drop=FALSE]) %*% centers[lrannames[[y]][notint_ran]]
            }
          }
        }
        # now increase the base dimension 
        yadd <- yadd + length(lrannames[[y]])
      }
    }
  }
  
  
  ### Sigma, InvSigma, sdSigma, corSigma, detInvSigma
  if(totnran > 0){
    # Create a scaling matrix L to rescale Sigma matrix with
    # does not depend on group-specificity of Sigma
    L <- matrix(0, totnran, totnran)
    yadd <- 0
    for(y in Ys){
      yscl <- ifelse(is.element(y,Nums), scales[y], 1.0)
      int_ran <- which(lrannames[[y]]=="(Intercept)") # could be empty
      notint_ran <- setdiff(seq_len(length(lrannames[[y]])), int_ran) # could be empty
      # non-intercept terms, if any
      for(z in notint_ran){
        L[z+yadd,z+yadd] <- yscl / scales[lrannames[[y]][z]]
      }
      # intercept
      if(length(int_ran) > 0){
        i <- int_ran+yadd
        L[i, i] <- yscl
        if(length(notint_ran) > 0){
          for(z in notint_ran){
            L[i, i+z] <- - centers[lrannames[[y]][z]] * yscl / scales[lrannames[[y]][z]]
          }
        }
      }
      # now increase the base dimension 
      yadd <- yadd + length(lrannames[[y]])
    }
    
    if(!all(L == diag(totnran))){
      # There are actually some changes to random effects 
      for(m in 1:length(draws$m)){
        if(settings["InvSigma","gspec"]){
          for(g in 1:settings["InvSigma","G"]){
            Sigma <- get_Sigma(draws, m, settings, g = g, howsave = "list")
            # perform rescaling and save new values
            newSigma <- L %*% Sigma %*% t(L)
            if(settings["Sigma","save"]){
              draws$Sigma[[g]][m,,] <- newSigma
            }
            if(settings["InvSigma","save"]){
              newInvSigma <- solve(newSigma)
              draws$InvSigma[[g]][m,,] <- newInvSigma
            }
            if(settings["sdSigma","save"] | settings["corSigma","save"]){
              newsdSigma <- sqrt(diag(newSigma))
            }
            if(settings["sdSigma","save"]){
              draws$sdSigma[[g]][m,] <- newsdSigma
            }
            if(settings["corSigma","save"] & (settings["corSigma","dims"] > 0)){
              newcorSigma <- diag(1/newsdSigma, nrow=totnran) %*% newSigma %*% diag(1/newsdSigma, nrow=totnran)
              draws$corSigma[[g]][m,,] <- newcorSigma
            }
            if(settings["detInvSigma", "save"]){
              draws$detInvSigma[m,g] <- 1/det(newSigma)
            }
          }# end for g in 1:G
        }else{
          # First get Sigma matrix for iteration m
          Sigma <- get_Sigma(draws, m, settings, howsave = "list")
          # perform rescaling and save new values
          newSigma <- L %*% Sigma %*% t(L)
          if(settings["Sigma","save"]){
            draws$Sigma[m,,] <- newSigma
          }
          if(settings["InvSigma","save"]){
            newInvSigma <- solve(newSigma)
            draws$InvSigma[m,,] <- newInvSigma
          }
          if(settings["sdSigma","save"] | settings["corSigma","save"]){
            newsdSigma <- sqrt(diag(newSigma))
          }
          if(settings["sdSigma","save"]){
            draws$sdSigma[m,] <- newsdSigma
          }
          if(settings["corSigma","save"] & (settings["corSigma","dims"] > 0)){
            newcorSigma <- diag(1/newsdSigma, nrow=totnran) %*% newSigma %*% diag(1/newsdSigma, nrow=totnran)
            draws$corSigma[m,,] <- newcorSigma
          }
          if(settings["detInvSigma", "save"]){
            draws$detInvSigma[m] <- 1/det(newSigma)
          }
          
        } # end else InvSigma gspec
      } # end for m 
    }else{
      # Can happen if there are no numeric outcomes and only random intercepts
    }
  }
  
  return(draws)
}
