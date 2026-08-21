


BIFACTOR <- function(loadings = NULL,
                     rawdata = NULL,   
                     cormat = NULL, Ncases = NULL, corkind = 'pearson', 
                     bifactor_kind = 'bifactorT', 
                     EFA_options    = list(extraction = 'minres', rotation = 'oblimin', Nfactors = 3),
                     schmid_options = list(extraction = 'minres', rotation = 'oblimin', N_group_factors = 3),
                     delta = .01,
                     min_loading = .2,
                     verbose = TRUE) { 
  
  # is the corkind method valid?
  if (!is.null(corkind)) {
    if (!corkind %in% c('pearson', 'kendall', 'spearman', 'gamma', 'polychoric')) {
      cat('\nThe entry for corkind,', corkind, ', is not one of the options for this function.')
      cat('\n"pearson" will be used instead.')
      corkind <- 'pearson'
    }
  }
  
  # is the bifactor_kind valid?
  if (!bifactor_kind %in% c('bifactorQ', 'bifactorT', 'bigeominT', 'bigeominQ', 
                            'SL', 'SLiD', 'DSL', 'none')) {
    cat('\nThe entry for bifactor_kind', bifactor_kind, 
        ', is not one of the options for this function.')
    cat('\n"bifactorT" will be used instead.')
    corkind <- 'bifactorT'
  }
  
  # check EFA_options if loadings = NULL
  if (is.null(loadings)) {
    # is the EFA_options extraction method valid?
    if (!EFA_options$extraction %in% c('paf', 'ml', 'image', 'minres', 'uls', 'ols', 'wls', 
                                       'gls', 'alpha', 'fullinfo')) {
      cat('\nThe EFA_options entry for extraction,', EFA_options$extraction, ', is not one of the options for this argument.')
      cat('\n"paf" will be used instead.')
      EFA_options$extraction <- 'paf'
    }
    
    # is the EFA_options rotation method valid?
    if (bifactor_kind %in% c('SL', 'SLiD', 'DSL')) {
      if (!EFA_options$rotation %in% c('bentlerQ', 'bentlerT', 
                                       'entropy', 'equamax', 'geominQ', 'geominT', 
                                       'oblimax', 'oblimin', 'promax', 'quartimax', 'quartimin', 
                                       'simplimax', 'varimax', 'none')) {
        cat('\nThe EFA_options entry for rotation,', EFA_options$rotation, ', is not one of the options for this function.')
        cat('\n"promax" will be used instead.')
        EFA_options$rotation <- 'promax'
      }
    }
    if (bifactor_kind %in% c('bifactorQ', 'bifactorT', 'bigeominT', 'bigeominQ')) {
      if (EFA_options$rotation != 'none') {
        cat('\nFor bifactor_kind = ', bifactor_kind, 'the EFA rotation must be, and will be changed to, "none"') 
        EFA_options$rotation <- 'none'
      }
    }
  }
  
  # check the schmid_options if schmid will be used
  if (bifactor_kind %in% c('SL', 'SLiD', 'DSL')) {
    
    # is the schmid_options extraction method valid?
    # all 3 of these bifactor methods use psych::schmid, which has restricted options
    # fm: the default is  minres. fm="pa" for principal axes, fm="pc" for principal 
    # components, fm = "minres" for minimum residual (OLS), pc="ml" for maximum likelihood
    
    if (!schmid_options$extraction %in% c('paf', 'minres', 'ml', 'pc')) {
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
  
  
  # run EFA when no loadings are provided; need raw data or cormat & Ncases
  if (is.null(loadings) & ( (!is.null(cormat) & !is.null(Ncases)) | !is.null(rawdata) )) {
    
    if (!is.null(cormat) & !is.null(Ncases)) {
      efa_output <- EFA(data = cormat, 
                        Nfactors   = EFA_options$Nfactors, 
                        extraction = EFA_options$extraction, 
                        rotation   = EFA_options$rotation, 
                        corkind = corkind, Ncases = Ncases, 
                        iterpaf=100, ppower = 3, delta = delta, verbose=FALSE)
    }
    
    if (!is.null(rawdata)) {
      efa_output <- EFA(data = rawdata, 
                        Nfactors   = EFA_options$Nfactors, 
                        extraction = EFA_options$extraction, 
                        rotation   = EFA_options$rotation, 
                        corkind = corkind, 
                        iterpaf=100, ppower = 3, delta = delta, verbose=FALSE)
      
      cormat <- cor(rawdata)
      
      Ncases = nrow(rawdata)
    }
    
    if (EFA_options$rotation == 'none')                                 loadings_for_BIF <- efa_output$loadingsNOROT
    
    if (EFA_options$rotation != 'none' & is.null(efa_output$pattern))   loadings_for_BIF <- efa_output$loadingsROT
    
    if (EFA_options$rotation != 'none' & !is.null(efa_output$pattern))  loadings_for_BIF <- efa_output$pattern
  }
  
  
  # no EFA because loadings were provided
  if (!is.null(loadings))  loadings_for_BIF <- loadings 
  
  
  # varexplROT <- loadingsROT <- structure <- pattern <- phi <- 
  #   loadingsBIF <- structureBIF <- patternBIF <- phiBIF <- NULL
  
  
  bif_output <- bifactor_engine(loadings = loadings_for_BIF, cormat = cormat, 
                                corkind = corkind, Ncases = Ncases, 
                                bifactor_kind = bifactor_kind, 
                                schmid_options = schmid_options,
                                delta = delta,
                                min_loading = min_loading) 
    
    if (verbose) {
      
      cat('\n\nBifactor results:')
 
      cat('\n\nbifactor_kind = ', bifactor_kind)
      
      # if (rotation == 'none')   cat('\nRotation procedure:  No initial rotation')
      # 
      # if (rotation != 'none') {
      
      # if (bifactor_kind_flag)
      #   cat('\nNfactors was increased by 1 for the', bifactor_kind, 
      #       'analyses, to incorporate the general factor.')
      
      if (is.null(bif_output$structureBIF)) {
        cat('\n\n\nBifactor Loadings:\n\n')	
        print(round(bif_output$loadingsBIF,2), print.gap=3) 
      }
      
      if (!is.null(bif_output$structureBIF)) { 
        
        cat('\n\n\n', paste(bifactor_kind, 
                            'Pattern Matrix (standardized factor loadings)'), '\n\n')	
        print(round(bif_output$loadingsBIF,2), print.gap=3)
        
        cat('\n\n\n', paste(bifactor_kind, 'Structure Matrix'), '\n\n')	
        print(round(bif_output$structureBIF,2), print.gap=3)
        
        cat('\n\n\n', paste(bifactor_kind, 'Factor Correlations:'), '\n\n')	
        print(round(bif_output$phiBIF,2), print.gap=3)
      }
      
      show_bifactor_stats(bif_output)
    }
  
  return(invisible(bif_output))
}

