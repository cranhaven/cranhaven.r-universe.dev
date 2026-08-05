rescale_matrix <- function(draws, settings, 
                           family, lfixnames, lgrpnames, lrannames,
                           Kord, Kcat,
                           centers, scales) {
  # outcomes by type
  Nums <- names(family)[is.element(family, c("num", "gaussian"))]
  Pois <- names(family)[is.element(family, c("poi", "poisson"))]
  Bins <- names(family)[is.element(family, c("bin", "bernoulli"))]
  Ords <- names(family)[is.element(family, c("ord", "cumulative"))]
  Cats <- names(family)[is.element(family, c("cat", "categorical"))]
  Ys <- c(Nums, Pois, Bins, Ords, Cats)
  
  ### beta_num, beta_num_fix, prec_num, sd_num, var_num
  # and naY
  for (y in Nums) {
    int_fix <- which(lfixnames[[y]] == "(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for (x in notint_fix) {
      bname <- paste0("beta_num_fix_", y, "[", x, "]")
      draws[, bname] <- as.numeric(draws[, bname] * scales[y]/ scales[lfixnames[[y]][x]])
    }
    
    int_grp <- which(lgrpnames[[y]] == "(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for (x in notint_grp) {
      for (g in 1:settings["beta_num", "G"]) {
        bname <- paste0("beta_num_", y, "(", g, ")[", x, "]")
        draws[, bname] <- as.numeric(draws[, bname] * scales[y] / scales[lgrpnames[[y]][x]])
      }
    }
    
    # precompute shift by beta_num_fix
    if (length(notint_fix) > 0) {
      bnames <- paste0("beta_num_fix_", y, "[", notint_fix, "]")
      shift_fix <- as.matrix(draws[, bnames, drop = FALSE]) %*%
          centers[lfixnames[[y]][notint_fix]]
    } else{
      shift_fix <- data.frame(x = rep(0, dim(draws)[1])) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if (length(int_fix) > 0) {
      # Intercept is among fixed effects
      bname <- paste0("beta_num_fix_", y, "[", int_fix, "]")
      draws[, bname] <- centers[y] + scales[y] * draws[, bname]
      if (length(notint_fix) > 0) {
        draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if (length(int_grp) > 0) {
      # Intercept is among group-specific effects
      for (g in 1:settings["beta_num", "G"]) {
        bname <- paste0("beta_num_", y, "(", g, ")[", int_grp, "]")
        draws[, bname] <- centers[y] + scales[y] * draws[, bname]
        # subtract scaled centers combined with beta_num_fix
        if (length(notint_fix) > 0) {
          draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
        }
        # subtract scaled centers combined with group-specific beta_num
        if (length(notint_grp) > 0) {
          bnames <- paste0("beta_num_", y, "(", g, ")[", notint_grp, "]")
          draws[, bname] <- as.numeric(draws[, bname] - as.matrix(draws[, bnames, drop = FALSE]) %*%
              centers[lgrpnames[[y]][notint_grp]])
        }
      }
    }
    
    # prec_num, sd_num, var_num
    for (p in c("prec_num", "sd_num", "var_num")) {
      if (settings[p, "save"]) {
        multiplier <- switch(p,
                             prec_num = 1/scales[y]^2,
                             sd_num = scales[y],
                             var_num = scales[y]^2)
        if (settings[p, "gspec"]) {
          for (g in 1:settings[p, "G"]) {
            pname <- paste0(p, "_", y, "(", g, ")")
            draws[, pname] <- as.numeric(draws[, pname] * multiplier)
          }
        }else{
          pname <- paste0(p, "_", y)
          draws[, pname] <- as.numeric(draws[, pname] * multiplier)
        }
      }
    }
    
    # naY
    if (settings["naY", "save"]) {
      if (settings["naY", "gspec"]) {
        for (g in 1:settings["naY", "G"]) {
          pnames <- grep(paste0("naY_", y, "\\(", g, "\\)"), colnames(draws))
          draws[, pnames] <- draws[, pnames] * scales[y] + centers[y]
        }
      }else{
        pnames <- grep(paste0("naY_", y), colnames(draws))
        draws[, pnames] <- draws[, pnames] * scales[y] + centers[y]
      }
    }
    
  } # end of for y in Nums
  
  ### beta_poi, beta_poi_fix
  for (y in Pois) {
    int_fix <- which(lfixnames[[y]] == "(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for (x in notint_fix) {
      bname <- paste0("beta_poi_fix_", y, "[", x, "]")
      draws[, bname] <- as.numeric(draws[, bname] / scales[lfixnames[[y]][x]])
    }
    
    int_grp <- which(lgrpnames[[y]] == "(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for (x in notint_grp) {
      for (g in 1:settings["beta_poi", "G"]) {
        bname <- paste0("beta_poi_", y, "(", g, ")[", x, "]")
        draws[, bname] <- as.numeric(draws[, bname] / scales[lgrpnames[[y]][x]])
      }
    }
    
    # precompute shift by beta_poi_fix
    if (length(notint_fix) > 0) {
      bnames <- paste0("beta_poi_fix_", y, "[", notint_fix, "]")
      shift_fix <- as.matrix(draws[, bnames, drop = FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- data.frame(x = rep(0, dim(draws)[1])) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if (length(int_fix) > 0) {
      # Intercept is among fixed effects
      bname <- paste0("beta_poi_fix_", y, "[", int_fix, "]")
      if (length(notint_fix) > 0) {
        draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if (length(int_grp) > 0) {
      # Intercept is among group-specific effects
      for (g in 1:settings["beta_poi", "G"]) {
        bname <- paste0("beta_poi_", y, "(", g, ")[", int_grp, "]")
        # subtract scaled centers combined with beta_poi_fix
        if (length(notint_fix) > 0) {
          draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
        }
        # subtract scaled centers combined with group-specific beta_poi
        if (length(notint_grp) > 0) {
          bnames <- paste0("beta_poi_", y, "(", g, ")[", notint_grp, "]")
          draws[, bname] <- as.numeric(draws[, bname] - as.matrix(draws[, bnames, drop = FALSE]) 
                                       %*% centers[lgrpnames[[y]][notint_grp]])
        }
      }
    }
  } # end of for y in Pois
  
  
  ### beta_bin, beta_bin_fix
  for (y in Bins) {
    int_fix <- which(lfixnames[[y]] == "(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for (x in notint_fix) {
      bname <- paste0("beta_bin_fix_", y, "[", x, "]")
      draws[, bname] <- as.numeric(draws[, bname] / scales[lfixnames[[y]][x]])
    }
    
    int_grp <- which(lgrpnames[[y]] == "(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for (x in notint_grp) {
      for (g in 1:settings["beta_bin", "G"]) {
        bname <- paste0("beta_bin_", y, "(", g, ")[", x, "]")
        draws[, bname] <- as.numeric(draws[, bname] / scales[lgrpnames[[y]][x]])
      }
    }
    
    # precompute shift by beta_bin_fix
    if (length(notint_fix) > 0) {
      bnames <- paste0("beta_bin_fix_", y, "[", notint_fix, "]")
      shift_fix <- as.matrix(draws[, bnames, drop = FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- data.frame(x = rep(0, dim(draws)[1])) # irrelevant
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if (length(int_fix) > 0) {
      # Intercept is among fixed effects
      bname <- paste0("beta_bin_fix_", y, "[", int_fix, "]")
      if (length(notint_fix) > 0) {
        draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if (length(int_grp) > 0) {
      # Intercept is among group-specific effects
      for (g in 1:settings["beta_bin", "G"]) {
        bname <- paste0("beta_bin_", y, "(", g, ")[", int_grp, "]")
        # subtract scaled centers combined with beta_bin_fix
        if (length(notint_fix) > 0) {
          draws[, bname] <- as.numeric(draws[, bname] - shift_fix)
        }
        # subtract scaled centers combined with group-specific beta_bin
        if (length(notint_grp) > 0) {
          bnames <- paste0("beta_bin_", y, "(", g, ")[", notint_grp, "]")
          draws[, bname] <- as.numeric(draws[, bname] - as.matrix(draws[, bnames, drop = FALSE]) 
                                       %*% centers[lgrpnames[[y]][notint_grp]])
        }
      }
    }
  } # end of for y in Bins
  
  
  ### beta_ord, beta_ord_fix, c_ord, a_ord, pi_ord
  for (y in Ords) {
    notint_fix <- seq_len(length(lfixnames[[y]])) # could be empty
    for (x in notint_fix) {
      bname <- paste0("beta_ord_fix_", y, "[", x, "]")
      draws[, bname] <- as.numeric(draws[, bname] / scales[lfixnames[[y]][x]])
    }
    
    notint_grp <- seq_len(length(lgrpnames[[y]])) # could be empty
    for (x in notint_grp) {
      for (g in 1:settings["beta_ord", "G"]) {
        bname <- paste0("beta_ord_", y, "(", g, ")[", x, "]")
        draws[, bname] <- as.numeric(draws[, bname] / scales[lgrpnames[[y]][x]])
      }
    }
    
    # precompute shift by beta_ord_fix
    if (length(notint_fix) > 0) {
      bnames <- paste0("beta_ord_fix_", y, "[", notint_fix, "]")
      shift_fix <- as.matrix(draws[, bnames, drop = FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
    }else{
      shift_fix <- matrix(rep(0, dim(draws)[1]), ncol = 1) # irrelevant
    }
    
    # For ordinal outcomes we have ordered intercepts c_ord
    if (settings["c_ord", "gspec"]) {
      for (g in 1:settings["c_ord", "G"]) {
        cnames <- paste0("c_ord_", y, "(", g, ")[", 1:Kord[y], "]")
        bnames <- paste0("beta_ord_", y, "(", g, ")[", notint_grp, "]")
        if (length(notint_grp) > 0) {
          shift <- shift_fix + as.matrix(draws[, bnames, drop = FALSE]) %*% centers[lgrpnames[[y]][notint_grp]]
        }else{
          shift <- shift_fix
        }
        for (cn in cnames) {
          draws[, cn] <- as.numeric(draws[, cn] + shift)
        }
        # logits
        logits <- matrix(0, nrow = dim(draws)[1], ncol = Kord[y] + 2)
        logits[, Kord[y]+2] <- 1
        for (k in 1:Kord[y]) {
          logits[, k+1] <- plogis(draws[, cnames[k]])
        }
        # a_ord
        if (settings["a_ord", "save"]) {
          anames <- paste0("a_ord_", y, "(", g, ")[", 1:Kord[y], "]")
          for (k in 1:Kord[y]) {
            draws[, anames[k]] <- as.numeric(log((logits[, k+1] - logits[, k])/(1-logits[, Kord[y]+1])))
          }
        }
        # pi_ord
        if (settings["pi_ord", "save"]) {
          pinames <- paste0("pi_ord_", y, "(", g, ")[", 1:(Kord[y]+1), "]")
          for (k in 1:(Kord[y]+1)) {
            draws[, pinames[k]] <- as.numeric(logits[, k+1] - logits[, k])
          }
        }
      }
    }else{
      cnames <- paste0("c_ord_", y, "[", 1:Kord[y], "]")
      # There should be no group-specific effects --> error already before
      # or c_ord has been made group-specific
      for (cn in cnames) {
        draws[, cn] <- as.numeric(draws[, cn] + shift_fix)
      }
      # logits
      logits <- matrix(0, nrow = dim(draws)[1], ncol = Kord[y] + 2)
      logits[, Kord[y]+2] <- 1
      for (k in 1:Kord[y]) {
        logits[, k+1] <- plogis(draws[, cnames[k]])
      }
      # a_ord
      if (settings["a_ord", "save"]) {
        anames <- paste0("a_ord_", y, "[", 1:Kord[y], "]")
        for (k in 1:Kord[y]) {
          draws[, anames[k]] <- as.numeric(log((logits[, k+1] - logits[, k])/(1-logits[, Kord[y]+1])))
        }
      }
      # pi_ord
      if (settings["pi_ord", "save"]) {
        pinames <- paste0("pi_ord_", y, "[", 1:(Kord[y]+1), "]")
        for (k in 1:(Kord[y]+1)) {
          draws[, pinames[k]] <- as.numeric(logits[, k+1] - logits[, k])
        }
      }
    } # end of if/else c_ord group-specific
  } # end of for y in Ords
  
  ### beta_cat, beta_cat_fix
  for (y in Cats) {
    int_fix <- which(lfixnames[[y]] == "(Intercept)") # could be empty
    notint_fix <- setdiff(seq_len(length(lfixnames[[y]])), int_fix) # could be empty
    for (x in notint_fix) {
      for (k in 1:Kcat[y]) {
        bname <- paste0("beta_cat_fix_", y, "[", x, ",", k, "]")
        draws[, bname] <- as.numeric(draws[, bname] / scales[lfixnames[[y]][x]])
      }
    }
    
    int_grp <- which(lgrpnames[[y]] == "(Intercept)") # could be empty
    notint_grp <- setdiff(seq_len(length(lgrpnames[[y]])), int_grp) # could be empty
    for (x in notint_grp) {
      for (g in 1:settings["beta_cat", "G"]) {
        for (k in 1:Kcat[y]) {
          bname <- paste0("beta_cat_", y, "(", g, ")[", x, ",", k, "]")
          draws[, bname] <- as.numeric(draws[, bname] / scales[lgrpnames[[y]][x]])
        }
      }
    }
    
    # precompute shift by beta_cat_fix
    shift_fix <- data.frame(matrix(0, nrow=dim(draws)[1], ncol=Kcat[y]))
    if (length(notint_fix) > 0) {
      for (k in 1:Kcat[y]) {
        bnames <- paste0("beta_cat_fix_", y, "[", notint_fix, ",", k, "]")
        shift_fix[, k] <- as.matrix(draws[, bnames, drop = FALSE]) %*% centers[lfixnames[[y]][notint_fix]]
      }
    }
    
    # rescale Intercept parameter if it is among fixed effects
    if (length(int_fix) > 0) {
      # Intercept is among fixed effects
      for (k in 1:Kcat[y]) {
        bname <- paste0("beta_cat_fix_", y, "[", int_fix, ",", k, "]")
        if (length(notint_fix) > 0) {
          draws[, bname] <- as.numeric(draws[, bname] - shift_fix[, k])
        }
      }
      # There cannot be any group-specific effects
      # The error should have already occurred or (Intercept) has been made group-specific
    }
    
    # rescale Intercept parameter if it is among group-specific effects
    if (length(int_grp) > 0) {
      # Intercept is among group-specific effects
      for (g in 1:settings["beta_cat", "G"]) {
        for (k in 1:Kcat[y]) {
          bname <- paste0("beta_cat_", y, "(", g, ")[", int_grp, ",", k, "]")
          # subtract scaled centers combined with beta_cat_fix
          if (length(notint_fix) > 0) {
            draws[, bname] <- as.numeric(draws[, bname] - shift_fix[, k])
          }
          # subtract scaled centers combined with group-specific beta_cat
          if (length(notint_grp) > 0) {
            bnames <- paste0("beta_cat_", y, "(", g, ")[", notint_grp, ",", k, "]")
            draws[, bname] <- as.numeric(draws[, bname] - as.matrix(draws[, bnames, drop = FALSE]) 
                                         %*% centers[lgrpnames[[y]][notint_grp]])
          }
        }
      }
    }
  } # end of for y in Cats
  
  
  ### Random effects b
  totnran <- sum(unlist(lapply(lrannames, length)))
  if (totnran > 0) {
    if (settings["b", "save"]) {
      yadd <- 0
      for (y in Ys) {
        yscl <- ifelse(is.element(y, Nums), scales[y], 1.0)
        int_ran <- which(lrannames[[y]] == "(Intercept)") # could be empty
        notint_ran <- setdiff(seq_len(length(lrannames[[y]])), int_ran) # could be empty
        # First, update random effects that are not intercepts
        for (z in notint_ran) {
          for (i in 1:settings["b", "d1"]) {
            bname <- paste0("b[", i, ",",z+yadd, "]")
            draws[, bname] <- as.numeric(draws[, bname] * yscl / scales[lrannames[[y]][z]])
          }
        }
        # Now update random intercepts (can be empty only if notint_ran is empty)
        if (length(int_ran) > 0) {
          for (i in 1:settings["b", "d1"]) {
            bname <- paste0("b[", i, ",", int_ran+yadd, "]")
            draws[, bname] <- as.numeric(yscl * draws[, bname])
            if (length(notint_ran) > 0) {
              bnames <- paste0("b[", i, ",", notint_ran+yadd, "]")
              draws[, bname] <- as.numeric(draws[, bname] - as.matrix(draws[, bnames, drop = FALSE]) %*%
                  centers[lrannames[[y]][notint_ran]])
            }
          }
        }
        # now increase the base dimension 
        yadd <- yadd + length(lrannames[[y]])
      }
    }
  }
  
  
  ### Sigma, InvSigma, sdSigma, corSigma, detInvSigma
  if (totnran > 0) {
    # Create a scaling matrix L to rescale Sigma matrix with
    # does not depend on group-specificity of Sigma
    L <- matrix(0, totnran, totnran)
    yadd <- 0
    for (y in Ys) {
      yscl <- ifelse(is.element(y, Nums), scales[y], 1.0)
      int_ran <- which(lrannames[[y]] == "(Intercept)") # could be empty
      notint_ran <- setdiff(seq_len(length(lrannames[[y]])), int_ran) # could be empty
      # non-intercept terms, if any
      for (z in notint_ran) {
        L[z+yadd,z+yadd] <- yscl / scales[lrannames[[y]][z]]
      }
      # intercept
      if (length(int_ran) > 0) {
        i <- int_ran+yadd
        L[i, i] <- yscl
        if (length(notint_ran) > 0) {
          for (z in notint_ran) {
            L[i, i+z] <- -centers[lrannames[[y]][z]] * yscl / scales[lrannames[[y]][z]]
          }
        }
      }
      # now increase the base dimension 
      yadd <- yadd + length(lrannames[[y]])
    }
    
    if (!all(L == diag(totnran))) {
      # There are actually some changes to random effects 
      if (settings["InvSigma", "gspec"]) {
        for (g in 1:settings["InvSigma", "G"]) {
          Sigmacols <- grep(paste0("^Sigma\\(", g, "\\)"), colnames(draws))
          InvSigmacols <- grep(paste0("^InvSigma\\(", g, "\\)"), colnames(draws))
          sdSigmacols <- grep(paste0("^sdSigma\\(", g, "\\)"), colnames(draws))
          corSigmacols <- grep(paste0("^corSigma\\(", g, "\\)"), colnames(draws))
          detInvSigmacols <- paste0("detInvSigma\\(", g, "\\)")
          for (m in 1:nrow(draws)) {
            Sigma <- get_Sigma(draws, m, settings, g = g, howsave = "data.frame",
                               Sigmacols = Sigmacols, InvSigmacols = InvSigmacols, 
                               sdSigmacols = sdSigmacols, corSigmacols = corSigmacols)
            # perform rescaling and save new values
            newSigma <- L %*% Sigma %*% t(L)
            if (settings["Sigma", "save"]) {
              draws[m,Sigmacols] <- newSigma[upper.tri(newSigma, diag = TRUE)]
            }
            if (settings["InvSigma", "save"]) {
              newInvSigma <- chol2inv(chol(newSigma))
              draws[m,InvSigmacols] <- newInvSigma[upper.tri(newInvSigma, diag = TRUE)]
            }
            if (settings["sdSigma", "save"] | settings["corSigma", "save"]) {
              newsdSigma <- sqrt(diag(newSigma))
            }
            if (settings["sdSigma", "save"]) {
              draws[m,sdSigmacols] <- newsdSigma
            }
            if (settings["corSigma", "save"] & (settings["corSigma", "dims"] > 0)) {
              newcorSigma <- diag(1/newsdSigma, nrow = totnran) %*%
                  newSigma %*% diag(1/newsdSigma, nrow = totnran)
              draws[m,corSigmacols] <- newcorSigma[upper.tri(newcorSigma, diag = FALSE)]
            }
            if (settings["detInvSigma", "save"]) {
              draws[m, detInvSigmacols] <- 1/det(newSigma)
            }
          } # end for m in 1:nrow(draws)
        }# end for g in 1:G
      }else{
        Sigmacols <- grep("^Sigma", colnames(draws))
        InvSigmacols <- grep("^InvSigma", colnames(draws))
        sdSigmacols <- grep("^sdSigma", colnames(draws))
        corSigmacols <- grep("^corSigma", colnames(draws))
        for (m in 1:nrow(draws)) {
          # First get Sigma matrix for iteration m
          Sigma <- get_Sigma(draws, m, settings, howsave = "data.frame",
                             Sigmacols = Sigmacols, InvSigmacols = InvSigmacols, 
                             sdSigmacols = sdSigmacols, corSigmacols = corSigmacols)
          # perform rescaling and save new values
          newSigma <- L %*% Sigma %*% t(L)
          if (settings["Sigma", "save"]) {
            draws[m,Sigmacols] <- newSigma[upper.tri(newSigma, diag = TRUE)]
          }
          if (settings["InvSigma", "save"]) {
            newInvSigma <- chol2inv(chol(newSigma))
            draws[m,InvSigmacols] <- newInvSigma[upper.tri(newInvSigma, diag = TRUE)]
          }
          if (settings["sdSigma", "save"] | settings["corSigma", "save"]) {
            newsdSigma <- sqrt(diag(newSigma))
          }
          if (settings["sdSigma", "save"]) {
            draws[m,sdSigmacols] <- newsdSigma
          }
          if (settings["corSigma", "save"] & (settings["corSigma", "dims"] > 0)) {
            newcorSigma <- diag(1/newsdSigma, nrow = totnran) %*% newSigma %*%
                diag(1/newsdSigma, nrow = totnran)
            draws[m,corSigmacols] <- newcorSigma[upper.tri(newcorSigma, diag = FALSE)]
          }
          if (settings["detInvSigma", "save"]) {
            draws[m, "detInvSigma"] <- 1/det(newSigma)
          }
          
        } # end for m 
      } # end else InvSigma gspec
    }else{
      # Can happen if there are no numeric outcomes and only random intercepts
    }
  }
  
  return(draws)
}
