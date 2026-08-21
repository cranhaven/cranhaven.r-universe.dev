

OMEGA <- function(data, corkind = 'pearson', 
                  bifactor_kind = c('McD', 'SL', 'SLiD', 'DSL', 
                                    'bifactorT', 
                                    'bigeominT' ),
                  EFA_options    = list(extraction = 'minres', rotation = 'oblimin', Nfactors = 3),
                  schmid_options = list(extraction = 'minres', rotation = 'oblimin', N_group_factors = 3),
                  delta = .01, min_loading = .2, display = 2) {
  
  # auto_reverse?
  
  if (display > 0)  cat('\n\nOMEGA:') 
    
  # is the corkind method valid?
  if (!corkind %in% c('pearson', 'kendall', 'spearman', 'gamma', 'polychoric')) {
    cat('\nThe entry for corkind,', corkind, ', is not one of the options for this function.')
    cat('\n"pearson" will be used instead.')
    corkind <- 'pearson'
  }
  
  # are the bifactor_kind valid?
  if (!all(bifactor_kind %in% c('McD', 'SL', 'SLiD', 'DSL', 
                                'bifactorQ', 'bifactorT', 'bigeominQ', 'bigeominT'))) {
    cat('\nThe entries for bifactor_kind', bifactor_kind,
        'contain values other than SL, SLiD, DSL, bifactorQ, bifactorT, bigeominQ, bigeominT.')
    cat('\n"bifactor_kind" will be set to the default (all methods).')
    bifactor_kind <- c('SL', 'SLiD', 'GPA', 'DSL', 
                       'bifactorT', 'bifactorQ', 'bigeominT', 'bigeominQ')
  }
  
  # is the EFA_options extraction method valid?
  if (!EFA_options$extraction %in% c('paf', 'ml', 'image', 'minres', 'uls', 'ols', 'wls', 
                                     'gls', 'alpha', 'fullinfo')) {
    cat('\nThe EFA_options entry for extraction,', EFA_options$extraction, ', is not one of the options for this argument.')
    cat('\n"paf" will be used instead.')
    EFA_options$extraction <- 'paf'
  }
  
  
  # check the schmid_options if schmid will be used
  if (any(bifactor_kind %in% c('SL', 'SLiD', 'DSL'))) {
    
    # is the schmid_options extraction method valid?
    # all 3 of these bifactor methods use psych::schmid, which has restricted options
    # fm: the default is  minres. fm="pa" for principal axes, fm="pc" for principal 
    # components, fm = "minres" for minimum residual (OLS), pc="ml" for maximum likelihood
    
    if (!schmid_options$extraction %in% c('paf', 'minres', 'ml')) {
      cat('\nThe schmid_options entry for extraction,', schmid_options$sextraction, ', is not one of the options for this argument.')
      cat('\n"minres" will be used instead.')
      schmid_options$extraction <- 'minres'
    }
    
    # is the schmid_options rotation method valid?
    # rotate: the default, oblimin, produces somewhat more correlated factors than the 
    # alternative, simplimax. Other options include Promax (not Kaiser normalized) 
    # or promax (Promax with Kaiser normalization). See fa for possible oblique rotations.
    
    if (!schmid_options$rotation %in% c('oblimin','simplimax','Promax','promax', 'none')) {   
      cat('\nThe schmid_options entry for rotation,', schmid_options$rotation, ', is not one of the options for this function.')
      cat('\n"oblimin" will be used instead.')
      schmid_options$rotation <- 'oblimin'
    }
  }
  
  # is EFA_options$Nfactors compatible with schmid_options$N_group_factors?
  # EFA_options$Nfactors should be = schmid_options$N_group_factors + 1
  # if not change EFA_options$Nfactors -- but it only matters if psych::schmid will be used
  
  if (any(bifactor_kind %in% c('SL', 'SLiD', 'DSL'))) {
    
    if (EFA_options$Nfactors != (schmid_options$N_group_factors + 1)) {
      
      EFA_options$Nfactors <- schmid_options$N_group_factors + 1
      
      cat('\n\nEFA_options$Nfactors was changed to',  EFA_options$Nfactors, 'to make it compatible\n') 
      cat('with schmid_options$N_group_factors')
    }
  }
  
  # EFA_options$Nfactors should be least 2, but 3 is better
  if (EFA_options$Nfactors == 1) {
    cat('\nThe analyses cannot be conducted when EFA_options$Nfactors = 1. It will be set to 3 instead.')
    EFA_options$Nfactors <- 3
  }
  
  
  data <- MISSING_DROP(data)
  
  # # reverse code every item that has a negative loading on the first principal component, BUT NOT IF 
  # # all of the loadings are negative 
  # if (auto_reverse) {
  #   new_data <- data
  #   cnoms <- colnames(data) 
  #   
  #   pc1 <- PCA(data, Nfactors = 1, rotation = 'none', verbose = FALSE)$loadingsNOROT
  #   
  #   if (!all(pc1 < 0) | !all(pc1 < 0) ) {
  #     
  #     for (lupe in 1:ncol(data))  {
  #       
  #       if (pc1[lupe,1] < 0)  {
  #         
  #         new_data[,lupe] <- REVERSE_CODE(item = data[,lupe], max_value = NULL)	
  #         
  #         message('\nItem ', cnoms[lupe], 
  #                 ' has been reverse-coded due to a negative loading on the first principal component')
  #         
  #         colnames(new_data)[lupe] <- paste(colnames(new_data)[lupe], "rev", sep="_")								
  #       }
  #     }
  #   }
  #   data <- new_data
  # }
  
  # set up cormat
  cordat <- setupcormat(data=data, corkind=corkind, Ncases=NULL)
  cormat <- cordat$cormat
  Ncases <- cordat$Ncases
  
  
  omega_total_McD <- 
    omega_total_SL <-           omega_hierl_SL <- 
    omega_total_SLiD <-         omega_hierl_SLiD <- 
    omega_total_DSL <-          omega_hierl_DSL <-           
    omega_total_bifactorT <-    omega_hierl_bifactorT <-      
    omega_total_bifactorQ <-    omega_hierl_bifactorQ <-      
    omega_total_bigeominT <-    omega_hierl_bigeominT <-      
    omega_total_bigeominQ <-    omega_hierl_bigeominQ <- 
    loadings_SL <-     
    loadings_SLiD <- 
    loadings_DSL <-    
    loadings_bifactorT <-  
    loadings_bifactorQ <-  
    loadings_bigeominT <-  
    loadings_bigeominQ <-  NULL
  
  outpmat <- c()
  
  
  if ('McD' %in% bifactor_kind) {
    
    # McDonald's omega - McNeish p 417 formula 2  -- using 1-factor EFA & no bifactor/S-L
    efa_output <- EFA.dimensions::EFA(data=cormat, 
                                      extraction = EFA_options$extraction, 
                                      rotation='none', 
                                      corkind=corkind, Ncases=Ncases, Nfactors = 1, 
                                      verbose=FALSE)
    
    loadings_McD <- efa_output$loadingsNOROT
    
    errors <- 1 - efa_output$communalities
    
    omega_total_McD <- sum(loadings_McD)**2 / (sum(loadings_McD)**2 + sum(errors))
    
    rmsr <- RMSR_boc(cormat, cormat_reproduced = reproduced_R(loadings_McD))
    
    outpmat <- rbind(outpmat, cbind(omega_total_McD, NA, NA, NA, NA, NA, rmsr, NA))
  }  
  
  
  bif_outp_SL <- bif_outp_SLiD <- bif_outp_DSL <- bif_outp_bifactorQ <-
    bif_outp_bifactorT <- bif_outp_bigeominQ <- bif_outp_bigeominT <- NULL
  
  for (lupe in 1:length(bifactor_kind)) {
    
    if (bifactor_kind[lupe] != 'McD') {
      
      # need unrotated loadings for 'bifactorT', 'bifactorQ', 'bigeominT', 'bigeominQ'
      if (bifactor_kind[lupe] %in% c('bifactorT', 'bifactorQ', 'bigeominT', 'bigeominQ')) {
        
        loadingsNOROT <- EFA(data=cormat, 
                             Nfactors = EFA_options$Nfactors, 
                             extraction = EFA_options$extraction, 
                             rotation = 'none', 
                             corkind = corkind, Ncases = Ncases, 
                             iterpaf=100, ppower = 3, delta = .01, 
                             verbose=FALSE)$loadingsNOROT 
      }
      
      bif_outp <- bifactor_engine(loadings = loadingsNOROT, cormat = cormat, 
                                  corkind = corkind, Ncases = Ncases, 
                                  bifactor_kind = bifactor_kind[lupe],
                                  schmid_options = schmid_options,
                                  min_loading = min_loading )  
      
      assign(paste("bif_outp_", bifactor_kind[lupe], sep=""),    bif_outp)
      
      assign(paste("loadings_", bifactor_kind[lupe], sep=""),    bif_outp$loadingsBIF)
      
      assign(paste("omega_total_", bifactor_kind[lupe], sep=""), bif_outp$omega_total)
      
      assign(paste("omega_hierl_", bifactor_kind[lupe], sep=""), bif_outp$omega_hierl)
      
      fitcoefs <- c(bif_outp$ECV[1], bif_outp$coef_H[1], bif_outp$FD[1], 
                    bif_outp$ARPB, bif_outp$rmsr, bif_outp$rmsr_gen)
      
      outpmat <- rbind(outpmat, cbind(bif_outp$omega_total[1], bif_outp$omega_hierl[1], t(fitcoefs)))
    }
  }
  
  rownames(outpmat) <- bifactor_kind
  colnames(outpmat) <- c('omega-T', 'omega-H', 'ECV', 'H', 'FD', 'ARPB', 'rmsr', 'rmsr-g')
  
  if (display > 0) {
    
    cat('\n\nNfactors = ', EFA_options$Nfactors)
    cat('\nNcases = ', Ncases) 
    cat('\nextraction = ', EFA_options$extraction)
    cat('\nrotation = ', EFA_options$rotation)
    cat('\ncorkind = ', corkind)
    
    cat('\n\nOmegas & bifactor model statistics:\n\n')
    print(round(outpmat,2), print.gap=3)
  }
  
  if (display == 2) {
    
    if ('SL' %in% bifactor_kind) {
      cat('\n\n\nSchmid-Leiman loadings:\n\n')
      print(round(loadings_SL,3), print.gap=4)
      show_bifactor_stats(bif_outp_SL)
    }
    
    if ('SLiD' %in% bifactor_kind) {
      cat('\n\n\nSLiD loadings:\n\n')
      print(round(loadings_SLiD,3), print.gap=4)
      show_bifactor_stats(bif_outp_SLiD)
    }
    
    if ('DSL' %in% bifactor_kind) {
      cat('\n\n\nDirect S-L loadings:\n\n')
      print(round(loadings_DSL,3), print.gap=4)
      show_bifactor_stats(bif_outp_DSL)
    }
    
    if ('bifactorT' %in% bifactor_kind) {
      cat('\n\n\nbifactorT loadings:\n\n')
      print(round(loadings_bifactorT,3), print.gap=4)
      show_bifactor_stats(bif_outp_bifactorT)
    }
    
    if ('bifactorQ' %in% bifactor_kind) {
      cat('\n\n\nbifactorQ loadings:\n\n')
      print(round(loadings_bifactorQ,3), print.gap=4)
      show_bifactor_stats(bif_outp_bifactorQ)
    }
    
    if ('bigeominT' %in% bifactor_kind) {
      cat('\n\n\nbigeominT loadings:\n\n')
      print(round(loadings_bigeominT,3), print.gap=4)
      show_bifactor_stats(bif_outp_bigeominT)
    }
    
    if ('bigeominQ' %in% bifactor_kind) {
      cat('\n\n\nbigeominQ loadings:\n\n')
      print(round(loadings_bigeominQ,3), print.gap=4)
      show_bifactor_stats(bif_outp_bigeominQ)
    }
  }
  
  output <- list(omega_total_McD       = omega_total_McD,
                 omega_total_SL        = omega_total_SL,          omega_hierl_SL         = omega_hierl_SL,
                 omega_total_SLiD      = omega_total_SLiD,        omega_hierl_SLiD       = omega_hierl_SLiD,
                 omega_total_DSL       = omega_total_DSL,         omega_hierl_DSL        = omega_hierl_DSL,           
                 omega_total_bifactorT = omega_total_bifactorT,   omega_hierl_bifactorT  = omega_hierl_bifactorT,     
                 omega_total_bifactorQ = omega_total_bifactorQ,   omega_hierl_bifactorQ  = omega_hierl_bifactorQ,     
                 omega_total_bigeominT = omega_total_bigeominT,   omega_hierl_bigeominT  = omega_hierl_bigeominT,     
                 omega_total_bigeominQ = omega_total_bigeominQ,   omega_hierl_bigeominQ  = omega_hierl_bigeominQ,     
                 loadings_SL           = loadings_SL,    
                 loadings_SLiD         = loadings_SLiD,
                 loadings_DSL          = loadings_DSL,   
                 loadings_bifactorT    = loadings_bifactorT, 
                 loadings_bifactorQ    = loadings_bifactorQ, 
                 loadings_bigeominT    = loadings_bigeominT, 
                 loadings_bigeominQ    = loadings_bigeominQ, 
                 cormat = cormat,
                 Ncases = Ncases,
                 outpmat = outpmat) 
  
  return(invisible(output))
}	



