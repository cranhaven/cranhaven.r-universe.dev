


ESEM <- function(data, Nfactors = NULL, extraction = 'ml', rotation = 'geominT',  
                 corkind = 'pearson', anchors = NULL, estimator = 'ML', delta = .01,
                 ordered = FALSE, target = NULL, target_keys = NULL, verbose = TRUE) {
  
  if (verbose)  cat('\n\nExploratory Structural Equation Modeling:')
  
  # is the extraction method valid?
  if (!extraction %in% c('paf', 'ml', 'image', 'minres', 'uls', 'ols', 'wls', 
                         'gls', 'alpha', 'fullinfo')) {
    cat('\nThe entry for extraction,', extraction, ', is not one of the options for this function.')
    cat('\n"paf" will be used instead.')
    extraction <- 'paf'
  }
  
  # is the rotation method valid?
  if (!rotation %in% c('bentlerQ', 'bentlerT', 'bifactorQ', 'bifactorT', 'bigeominQ', 
                       'bigeominT', 'entropy', 'equamax', 'geominQ', 'geominT', 
                       'oblimax', 'oblimin', 'promax', 'quartimax', 'quartimin', 
                       'simplimax', 'varimax', 'targetQ', 'targetT', 'none')) {
    cat('\nThe entry for rotation,', rotation, ', is not one of the options for this function.')
    cat('\n"promax" will be used instead.')
    rotation <- 'promax'
  }
  
  # is the corkind method valid?
  if (!corkind %in% c('pearson', 'kendall', 'spearman', 'gamma', 'polychoric')) {
    cat('\nThe entry for corkind,', corkind, ', is not one of the options for this function.')
    cat('\n"pearson" will be used instead.')
    corkind <- 'pearson'
  }
  
  data <- MISSING_DROP(data)
  
  N_obsvd_vars  <- ncol(data)
  varnames <- colnames(data)
  
  # does the estimator work for ordered data?
  if (ordered & !estimator %in% c('WLSMV', 'DWLS', 'WLS', 'ULS', 'ULSMV')) {
    cat('\nThe', estimator, 'estimator does not work for ordered data. WLSMV will be used instead.')
    estimator <- 'WLSMV'
  }
  
  if (is.null(Nfactors)) {		
    Nfactors <- EMPKC(data=data, corkind=corkind, verbose=FALSE)$NfactorsEMPKC
    NfactorsWasNull <- TRUE
  } else {NfactorsWasNull <- FALSE}
  
  if (!is.null(target_keys)) {
    
    if (!is.null(target)) 
      cat('\n\ntarget_keys were specified but so was target. The target_keys will be ignored')
    
    if (is.null(target)) {
      
      # check for errors in target_keys
      if (length(target_keys) != N_obsvd_vars)
        cat('\nThe length of target_keys is not equal to the number of variables in data.')
      if (min(target_keys) != 1)  cat('\nThe smallest number in target_keys is not 1')
      if (!all(seq(min(target_keys):max(target_keys)) %in% unique((target_keys))))
        cat('\nNot all of the possible factor numbers appear in target_keys')
      
      target <- matrix(0, length(target_keys), max(target_keys))
      for (lupe in 1:length(target_keys))  target[lupe, target_keys[lupe]] <- NA
      rownames(target) <- varnames
      colnames(target) <- paste('Factor_', 1:ncol(target), sep = '')
    }
  }
  
  if (is.null(target)) {
    
    if (rotation %in% c('targetQ', 'targetT')) {
      cat('\n\nRotation was set to', rotation, 'but a target was provided.')
      cat('\nRotation was therefore set to "promax" for the EFA.')
      rotation <- "promax"
    }
    
    efa_output <- EFA(data=data, extraction = extraction, corkind=corkind,  
                      Nfactors=Nfactors, rotation=rotation, delta = delta, verbose=FALSE)
    
    if (all(!is.na(efa_output$pattern)))  loadmat <- efa_output$pattern
    if (all( is.na(efa_output$pattern)))  loadmat <- efa_output$loadingsROT
  }
  
  if (!is.null(target)) {
    
    if (!rotation %in% c('targetQ', 'targetT')) {
      cat('\n\nA target was provided and so rotation was set to "targetQ" for the EFA.')
      rotation <- "targetQ"
    }
    
    # make sure the size of target is correct
    if (ncol(target) != Nfactors) {
      cat('\nThe number of columns in target', ncol(target), 'was not = Nfactors')
      cat('\nNfactors was therefore changed to the number of columns in target')
      Nfactors <- ncol(target)
    }
    if (nrow(target) != N_obsvd_vars)
      stop('The number of rows in target', ncol(target),
           'was not = to the number of variables in data,', N_obsvd_vars)
    
    # target rotation using psych::fa seems to require that target is a list, not a matrix
    # efa_output <- psych::fa(r = data, nfactors = Nfactors, fm = extraction,
    #                         rotate = rotation, Target = target)
    #                         # scores=scores,
    #                         # residuals=residuals,
    #                         # missing=missing)
    # 
    # loadmat <- efa_output$loadings
    
    # first run an efa with no rotation
    efa_output <- EFA(data=data, extraction = extraction, corkind=corkind,  
                      Nfactors=Nfactors, rotation='none', verbose=FALSE)$loadingsNOROT
    
    # use GPArotation for target rotation
    if (rotation == "targetQ")  loadmat <- targetQ(efa_output, Target = target)$loadings
    if (rotation == "targetT")  loadmat <- targetT(efa_output, Target = target)$loadings
  }
  
  
  colnames(loadmat) <- paste('Factor_', 1:ncol(loadmat), sep = '')
  
  
  # # loadmat <- zapsmall(matrix(round(efa_output, 2), nrow = N_obsvd_vars, ncol = Nfactors))
  # loadmat <- zapsmall(matrix(efa_output, nrow = N_obsvd_vars, ncol = Nfactors))
  # rownames(loadmat) <- varnames
  
  
  # if anchors are provided, test if they are valid rownames
  if (!is.null(anchors)) {
    if (!all(anchors %in% varnames)) 
      stop('anchors were specified but they do not all appear in the variable names')
  }
  # if anchors are not provided, id them - the highest-loading item on each factor
  if (is.null(anchors)) 
    anchors <- rownames(loadmat)[apply(loadmat, 2, function(x) which(abs(x) == max(abs(x))))]
  
  # create a set of lavaan equations from the efa loadings
  # use the efa loadings as start values, except do not use "start" for the anchor items
  terms  <- vector()
  for (lupe in 1:Nfactors) {
    
    dum <- paste0("F", lupe,"=~")
    
    for (lupe_rows in 1:nrow(loadmat)) {
      
      if (lupe_rows != nrow(loadmat)) {
        
        if (rownames(loadmat)[lupe_rows] %in% anchors)  dum <- 
            paste0(dum, paste0(loadmat[lupe_rows,lupe], "*", rownames(loadmat)[lupe_rows], "+\n"))
        if (!rownames(loadmat)[lupe_rows] %in% anchors)  dum <- 
            paste0(dum, paste0('start(', loadmat[lupe_rows,lupe], ")*", rownames(loadmat)[lupe_rows], "+\n"))
      } 
      
      if (lupe_rows == nrow(loadmat)) {
        
        if (rownames(loadmat)[lupe_rows] %in% anchors)  dum <- 
            paste0(dum, paste0(loadmat[lupe_rows,lupe], "*", rownames(loadmat)[lupe_rows], "\n"))
        if (!rownames(loadmat)[lupe_rows] %in% anchors)  dum <- 
            paste0(dum, paste0('start(', loadmat[lupe_rows,lupe], ")*", rownames(loadmat)[lupe_rows], "\n"))
      } 
    }     
    terms[lupe] <-dum
  }
  
  esem_model_syntax <- paste(terms, collapse = "\n")
  
  # writeLines(esem_model_syntax)
  
  
  # # "Correlated uniqueness"
  # terms[6] <- "A1 ~~ C2+E3+N3\n C2 ~~ E3+N3\n E3 ~~ N3"
  
  
  lavaan_output <- lavaan::cfa(esem_model_syntax, data=data, estimator=estimator, #  "WLSMV", 
                               std.lv=TRUE, ordered=ordered, verbose=FALSE)
  
  # fits <- lavaan::fitmeasures(lavaan_output, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
  # print(fits)
  
  
  if (verbose) {
    
    cat('\n\nThe number of useable cases in data:', nrow(data))
    
    cat('\n\nThe number of variables:', N_obsvd_vars)
    
    cat('\n\nThe efa extraction method:', extraction)
    
    if (!rotation %in% c('targetQ','targetT')) cat('\n\nThe efa rotation method:', rotation)
    
    if (rotation == 'targetQ')  cat('\n\nThe efa rotation method:', rotation, ' (oblique)')
    
    if (rotation == 'targetT')  cat('\n\nThe efa rotation method:', rotation, ' (orthogonal)')
    
    cat('\n\nThe kind of correlations for the efa:', corkind,'\n')
    
    if (NfactorsWasNull ) {
      cat('\nNfactors was not specified and so the EMPKC test was')
      cat('\nconducted to determine the number of factors to extract: Nfactors = ', Nfactors)		
    } else if (!NfactorsWasNull) {
      cat('\nThe number of factors: ', Nfactors)
    }
    
    if (!is.null(target)) {
      cat('\n\nThe target loading matrix:\n\n')
      print(round(target,3))
    }
    
    cat('\n\nThe efa loadings:\n\n')
    print(round(loadmat,3))
    
    cat('\n\nThe identified anchors: ', anchors, '\n')
    
    cat('\n\nESEM results: \n\n')
    print(lavaan::summary(lavaan_output, fit.measures=TRUE, standardized=TRUE))
    
    # lavaanPlot(model = lavaan_output, coefs = TRUE,
    #            stand = TRUE,
    #            edge_options = list(color ='grey'))
    
  }
  
  output <- list(loadings = loadmat, esem_model_syntax = esem_model_syntax, 
                 anchors = anchors, lavaan_output = lavaan_output)
  
  return(invisible(output))
}

