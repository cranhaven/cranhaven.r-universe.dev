

PA_FA      <- function( ... ) { .Defunct("EFA", package="EFA.dimensions") }

MAXLIKE_FA <- function( ... ) { .Defunct("EFA", package="EFA.dimensions") }

IMAGE_FA   <- function( ... ) { .Defunct("EFA", package="EFA.dimensions") }



EFA <- function (data, Nfactors=NULL, extraction = 'paf', rotation='promax', 
                 corkind='pearson', Ncases=NULL, 
                 iterpaf=100, ppower = 3, delta = .01, verbose=TRUE) {
  
  # is the extraction method valid?
  if (!extraction %in% c('paf', 'ml', 'image', 'minres', 'uls', 'ols', 'wls', 
                         'gls', 'alpha', 'fullinfo')) {
    cat('\nThe entry for extraction,', extraction, ', is not one of the options for this function.')
    cat('\n"paf" will be used instead.')
    extraction <- 'paf'
  }
  
  # is the rotation method valid?
  if (!rotation %in% c('bentlerQ', 'bentlerT', 
                       'entropy', 'equamax', 'geominQ', 'geominT', 
                       'oblimax', 'oblimin', 'promax', 'quartimax', 'quartimin', 
                       'simplimax', 'varimax', 'none')) {
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
  
  cnoms <- colnames(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  Nvars <- dim(cormat)[1]
  
  eigenvalues <- eigen(cormat)$values
  
  varexplNOROT1 <- VarianceExplained(eigenvalues)
  
  # Ratio of the 1st to the 2nd initial eigenvalues
  evals12ratio <- eigenvalues[1] / eigenvalues[2]
  
  if (is.null(Nfactors)) {		
    Nfactors <- EMPKC(cormat, Ncases=Ncases, verbose=FALSE)$NfactorsEMPKC
    NfactorsWasNull <- TRUE
  } else {NfactorsWasNull <- FALSE}
  
  
  ####################################  extraction  ###################################
  
  # get the unrotated loadings for the specified extraction method
  
  if (extraction == 'paf') {
    outp <- PA_FA(cormat=cormat, Nfactors=Nfactors, Ncases=Ncases, iterpaf=iterpaf)
    loadingsNOROT <- outp$loadingsNOROT
    communalities <- outp$communalities
  }
  
  if (extraction == 'ml') {
    outp <- MAXLIKE_FA(cormat=cormat, Nfactors=Nfactors, Ncases=Ncases)
    loadingsNOROT <- outp$loadingsNOROT
    communalities <- outp$communalities
  }
  
  if (extraction == 'image') {
    outp <- IMAGE_FA(cormat=cormat, Nfactors=Nfactors, Ncases=Ncases)
    loadingsNOROT <- outp$loadingsNOROT
    communalities <- outp$communalities
  }
  
  if (extraction == 'minres') {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='minres', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communalities
  }
  
  if (extraction == 'uls')  {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='uls', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communalities
  }
  
  if (extraction == 'ols')  {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='ols', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communalities
  }
  
  if (extraction == 'wls')  {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='wls', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communalities
  }
  
  if (extraction == 'gls')  {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='gls', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communalities
  }
  
  if (extraction == 'alpha')  {
    outp <- psych::fa(r=cormat, nfactors=Nfactors, n.obs=Ncases, fm='alpha', rotate='none')
    loadingsNOROT <- outp$loadings[1:dim(outp$loadings)[1], 1:dim(outp$loadings)[2], drop=FALSE]
    communalities <- outp$communality  # an apparent name error in psych  communality rather than communalities
  }
  
  if (extraction == 'fullinfo' & cordat$datakind == "notcorrels")  {	
    # run only if data consists of integers
    # confirm that the check the fractional parts of each # in data is 0  (better than using is.integer)
    if (all(data %%1 == 0)) {				
      mirt_mod <- mirt::mirt(data=data, item_type=NULL, Nfactors)    # , itemtype = "graded"
      sum_mirt_mod <- mirt::summary(mirt_mod, rotate = 'varimax')
      loadingsNOROT <- sum_mirt_mod$rotF
      communalities <- sum_mirt_mod$h2
      # # mirt sometimes produces 0 values for factor loadings; changing to a v small #		
      # loadingsNOROT <- ifelse(loadingsNOROT == 0, .0000001, loadingsNOROT)
      
    } else {
      message('\n\n"data" does not consist of integers and so a full-information factor analysis cannot')
      message('be conducted. A paf extraction will be used instead.\n')
      extraction <- 'paf'
      outp <- PA_FA(cormat=cormat, Nfactors=Nfactors, Ncases=Ncases, iterpaf=iterpaf)
      loadingsNOROT <- outp$loadingsNOROT
      communalities <- outp$communalities
    }		
  }
  
  rownames(loadingsNOROT) <- cnoms
  colnames(loadingsNOROT) <- c(paste('Factor ', 1:Nfactors, sep=''))
  
  varexplNOROT2 <- VarianceExplained(eigenvalues, loadingsNOROT = loadingsNOROT)
  

  ####################################  rotation  ###################################
  
  varexplROT <- loadingsROT <- structure <- pattern <- phi <- NULL
  
  if (Nfactors > 1) {
    
    # orthogonal rotations
    
    if (rotation == 'bentlerT') 
      loadingsROT <- GPArotation::bentlerT(loadingsNOROT)$loadings
    
    if (rotation == 'entropy') 
      loadingsROT <- GPArotation::entropy(loadingsNOROT)$loadings
    
    if (rotation == 'equamax') 
      loadingsROT <- psych::equamax(loadingsNOROT)$loadings
    
    if (rotation == 'geominT') 
      loadingsROT <- GPArotation::geominT(loadingsNOROT, delta=delta)$loadings
    
    if (rotation == 'quartimax') 
      loadingsROT <- GPArotation::quartimax(loadingsNOROT)$loadings
    
    if (rotation == 'varimax') 
      loadingsROT <- VARIMAX(loadingsNOROT, verbose=FALSE)$loadingsV
    
    # GPArotation::parsimax(loadings)   not an exported object from 'namespace:GPArotation'
    
    
    # oblique rotations
    
    if (rotation == 'bentlerQ') {
      outp <- GPArotation::bentlerQ(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'geominQ') {
      outp <- GPArotation::geominQ(loadingsNOROT, delta=delta)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'oblimin') {
      outp <- GPArotation::oblimin(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'oblimax') {
      outp <- GPArotation::oblimax(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'promax' | rotation == 'PROMAX') {
      promaxOutput <- PROMAX(loadingsNOROT, ppower=ppower, verbose=FALSE)
      pattern <- promaxOutput$pattern
      structure <- promaxOutput$structure
      phi <- promaxOutput$phi
    }
    
    if (rotation == 'quartimin') {
      outp <- GPArotation::quartimin(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'simplimax') {
      outp <- GPArotation::simplimax(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
  }
  
  
  # variance explained by the factors
  if (!is.null(loadingsROT)) 
    varexplROT <- VarianceExplained(eigenvalues, loadingsNOROT = loadingsNOROT,
                                         loadingsROT = loadingsROT)

  if (!is.null(structure)) 			
    varexplROT <- VarianceExplained(eigenvalues, loadingsNOROT = loadingsNOROT,
                                         loadingsROT = pattern, phi = phi)

  
  # communalities & uniquenesses
  if (!is.matrix(communalities))  communalities <- as.matrix(communalities)
  rownames(communalities) <- cnoms
  if (ncol(communalities) == 1)  colnames(communalities) <- c('Communalities')
  if (ncol(communalities) == 2)  colnames(communalities) <- c('Initial', 'Extraction')
  uniquenesses <- 1 - communalities
  
  
  
  #############################  factor model stats   ###############################
  
  fit_coefs <- FIT_COEFS(cormat=cormat, loadings=loadingsNOROT, 
                         extraction=extraction, Ncases=Ncases, verbose=FALSE) 

 
  efaOutput <- list(loadingsNOROT = loadingsNOROT,
                    loadingsROT = loadingsROT,
                    pattern = pattern,
                    structure = structure, 
                    phi = phi,
                    varexplNOROT1 = varexplNOROT1,
                    varexplNOROT2 = varexplNOROT2,
                    varexplROT = varexplROT,
                    evals12ratio = evals12ratio,
                    communalities = communalities,
                    uniquenesses = uniquenesses,
                    fit_coefs = fit_coefs)
  
  
  if (verbose ) {
    
    cat('\n\n\nExploratory Factor Analysis')
    
    cat('\n\nThe specified kind of factor extraction method for this analysis: ', extraction)
    
    cat('\n\nThe specified kind of correlations for this analysis: ', ctype)
    if (NfactorsWasNull ) {
      cat('\n\nNfactors was not specified and so the EMPKC test was conducted to determine')
      cat('\nthe number of factors to extract: Nfactors = ', Nfactors,'\n')		
    } else if (!NfactorsWasNull) {
      cat('\n\nThe specified number of factors to extraction = ', Nfactors,'\n')
    }
    
    cat('\nCommunalities:\n\n')
    print(round(communalities,2))
    
    cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
    cat('               Initial\n')  
    print(varexplNOROT1, print.gap=2)
    
    cat('\nRatio of the 1st to the 2nd initial eigenvalues = ', round(evals12ratio,1))
    
    cat('\n\n\nModel Fit:\n')
    
    cat('\n    Chi square = ', round(fit_coefs$chisqMODEL,2),
            '   df = ', fit_coefs$dfMODEL,'    p = ', round(fit_coefs$pvalue,5), '\n')
    
    if (extraction %in% c('IMAGE','image')) {  
      cat('\n    RMSR = ',  round(fit_coefs$RMSR,3))
      cat('\n    GFI = ',   round(fit_coefs$GFI,3))
      cat('\n    CAF = ',   round(fit_coefs$CAF,3))
    }
    
    if (!extraction %in% c('IMAGE','image')) {  
      
      cat('\n     RMSR =',  format(round(fit_coefs$RMSR,3), nsmall = 3), 
          '     RMSEA =', format(round(fit_coefs$RMSEA,3), nsmall = 3),
          '     BIC =', round(fit_coefs$BIC,3))
      
      cat('\n     GFI = ',  format(round(fit_coefs$GFI,3), nsmall = 3), 
          '     TLI  = ', format(round(fit_coefs$TLI,3), nsmall = 3),
          '     AIC =', round(fit_coefs$AIC,3))
      
      cat('\n     CAF = ',  format(round(fit_coefs$CAF,3), nsmall = 3), 
          '     CFI  = ', format(round(fit_coefs$CFI,3), nsmall = 3),
          '     CAIC =', round(fit_coefs$CAIC,3))
      
      cat('\n', 
          '                      MFI  = ', round(fit_coefs$MFI,3),
          '     SABIC =', round(fit_coefs$SABIC,3))
    }    
    
    cat('\n\nUnrotated Loadings:\n\n')
    print(round(loadingsNOROT,2), print.gap=3)
    
    cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n')
    cat('\n               Initial           Unrotated\n')  
    print(efaOutput$varexplNOROT2, print.gap=2)

    if (Nfactors == 1)  cat('\nNo rotation because there is only one factor\n') 
    
    if (rotation == 'none')   cat('\nRotation procedure:  No rotation')
    
    if (rotation != 'none' & Nfactors > 1) {    
      
      cat('\n\nThe specified rotation procedure: ', rotation)	
      
      if (!is.null(loadingsROT)) {
        cat('\n\n\nRotated Loadings:\n\n')	
        print(round(efaOutput$loadingsROT,2), print.gap=3) 
      }
      
      if (!is.null(pattern)) { 
        
        cat('\n\n\nPattern Matrix (standardized factor loadings):\n\n');      
        print(round(efaOutput$pattern,2), print.gap=3)
        
        cat('\n\nStructure Matrix:\n\n');    
        print(round(efaOutput$structure,2), print.gap=3)
        
        cat('\n\nFactor Correlations:\n\n'); 
        print(round(efaOutput$phi,2), print.gap=3)
      }
      
      cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n')
      cat('\n               Initial           Unrotated            Rotated\n')  
      print(efaOutput$varexplROT, print.gap=2)
    }
  }
  
  return(invisible(efaOutput))
}






