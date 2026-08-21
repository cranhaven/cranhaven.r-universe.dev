


DIMTESTS <- function(data, tests=c('EMPKC', 'HULL', 'RAWPAR'), 
                     corkind='pearson', Ncases=NULL, 
                     HULL_method='PAF', HULL_gof='CAF', HULL_cor_method='pearson',
                     CD_cor_method='pearson', display = 2) {
  
  # check tests
  if (!all(tests %in% c('CD', 'EMPKC', 'HULL', 'MAP', 'NEVALSGT1', 
                        'RAWPAR', 'SALIENT', 'SESCREE', 'SMT'))) {
    cat('\nOne or more values for tests is not a possible option. tests has been changed to the default.')
    tests <- c('EMPKC', 'HULL', 'RAWPAR')
  }
  
  # check corkind 
  if (!corkind %in% c('pearson', 'kendall', 'spearman', 'gamma', 'polychoric')) {
    cat('\nThe entry for corkind (', corkind, ') is not one of the options for this function.', sep='')
    cat('\n"pearson" will be used instead.')
    corkind <- 'pearson'
  }
  
  # check HULL_method
  if (!HULL_method %in% c('PAF', 'ML', 'ULS')) {
    cat('\nThe entry for HULL_method (', HULL_method, ') is not one of the options for this function.', sep='')
    cat('\n"PAF" will be used instead.')
    HULL_method <- 'PAF'
  }
  
  # check HULL_gof
  if (!HULL_gof %in% c('CAF', 'CFI', 'RMSEA')) {
    cat('\nThe entry for HULL_gof (', HULL_gof, ') is not one of the options for this function.', sep='')
    cat('\n"CAF" will be used instead.')
    HULL_gof <- 'CAF'
  }
  
  # check HULL_cor_method
  if (!HULL_cor_method %in% c('pearson', 'kendall', 'spearman')) {
    cat('\nThe entry for HULL_cor_method (', HULL_cor_method, ') is not one of the options for this function.', sep='')
    cat('\n"pearson" will be used instead.')
    HULL_cor_method <- 'pearson'
  }
  
  # check CD_cor_method
  if (length(CD_cor_method) > 1) {
    cat('\nCD_cor_method can have only one value. It has been changed to ', CD_cor_method[1])
    CD_cor_method <- CD_cor_method[1]
  }
  if (!CD_cor_method %in% c("pearson", "spearman", "kendall")) {
    cat('\nCD_cor_method must be one of pearson, spearman, or kendall. It has been changed to pearson')
    CD_cor_method <- 'pearson'
  }
  
  
  data <- MISSING_DROP(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  verbose <- ifelse(display == 2, TRUE, FALSE) 
  
  dimtests <- matrix(-9999,length(tests),1)
  
  for (lupe in 1:length(tests)) {
    
    if (tests[lupe] == 'CD') {		
      dimtests[lupe,1] <- EFAtools::CD(x=data, n_factors_max = NA, N_pop = 10000, 
                                       N_samples = 500, alpha = 0.3,
                                       cor_method = CD_cor_method, max_iter = 50)$n_factors
      if (verbose)	 {
        cat('\n\nCOMPARISON DATA')
        cat('\n\nThe number of factors according to the Comparison Data test (from EFAtools) = ', 
                dimtests[lupe,1], '\n')
      }
    }
    
    if (tests[lupe] == 'EMPKC') dimtests[lupe,1] <- 
        EMPKC(cormat, Ncases=Ncases, verbose=verbose)$NfactorsEMPKC
    
    if (tests[lupe] == 'HULL') {
      hullOutput <- suppressWarnings(
        EFAtools::HULL(x=cormat, N = Ncases, method = HULL_method, 
                       gof = HULL_gof, 
                       eigen_type = c("PCA"), 
                       cor_method = HULL_cor_method, 
                       n_datasets = 500, percent = 50, decision_rule = c("means") ))
      
      if (HULL_method == 'PAF')  dimtests[lupe,1] <- hullOutput$n_fac_CAF
      
      if ( (HULL_method == 'ML' | HULL_method == 'ULS') & HULL_gof=='CFI')    
        dimtests[lupe,1] <- hullOutput$n_fac_CFI
      
      if ( (HULL_method == 'ML' | HULL_method == 'ULS') & HULL_gof=='CAF')    
        dimtests[lupe,1] <- hullOutput$n_fac_CAF
      
      if ( (HULL_method == 'ML' | HULL_method == 'ULS') & HULL_gof=='RMSEA')  
        dimtests[lupe,1] <- hullOutput$n_fac_RMSEA	
      
      if (verbose)	 {
        cat('\n\nHULL METHOD')
        cat('\n\nThe number of factors according to the Hull method test (from EFAtools) = ', 
                dimtests[lupe,1], '\n')
      }
    }
    
    if (tests[lupe] == 'MAP') dimtests[lupe,1] <- 
        MAP(cormat, Ncases=Ncases, verbose=verbose)$NfactorsMAP
    
    if (tests[lupe] == 'NEVALSGT1') dimtests[lupe,1] <- 
        NEVALSGT1(cormat, Ncases=Ncases, verbose=verbose)$NfactorsNEVALSGT1
    
    if (tests[lupe] == 'RAWPAR') dimtests[lupe,1] <- 
        RAWPAR(cormat, Ncases=Ncases, verbose=verbose)$NfactorsPA
    
    if (tests[lupe] == 'SALIENT') dimtests[lupe,1] <- 
        SALIENT(cormat, Ncases=Ncases, verbose=verbose)$NfactorsSALIENT
    
    if (tests[lupe] == 'SESCREE') dimtests[lupe,1] <- 
        SESCREE(cormat, Ncases=Ncases, verbose=verbose)$NfactorsSESCREE
    
    if (tests[lupe] == 'SMT') dimtests[lupe,1] <- 
        SMT(cormat, Ncases=Ncases, verbose=verbose)$NfactorsSMT
  }
  rownames(dimtests) <- tests
  colnames(dimtests) <- '# of Factors:'
  
  if (display > 0) {
    cat('\n\nDIMTESTS results:\n')
    print(dimtests)
  }	
  
  dimtestOutput <- list(dimtests=dimtests, NfactorsDIMTESTS = dimtests[1,1])
  
  return(invisible(dimtestOutput))
}

