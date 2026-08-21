
# Principal Components Analysis

PCA <- function (data, Nfactors=NULL, rotation='promax', corkind='pearson', 
                 Ncases=NULL, ppower = 3, delta = .01, 
                 verbose=TRUE, rotate) {
  
  # deprecated  
  if (!missing(rotate))  rotation <- rotate
  
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
  
  cnoms <- colnames(data) # get colnames
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  eigs         <- eigen(as.matrix(cormat))
  eigenvalues  <- eigs$values
  eigenvectors <- eigs$vectors
  
  varexplNOROT <- VarianceExplained(eigenvalues)
  
  # Ratio of the 1st to the 2nd initial eigenvalues
  evals12ratio <- eigenvalues[1] / eigenvalues[2]
  
  
  if (is.null(Nfactors)) {		
    Nfactors <- EMPKC(cormat, Ncases=Ncases, verbose=FALSE)$NfactorsEMPKC
    NfactorsWasNull <- TRUE
  } else {NfactorsWasNull <- FALSE}
  
  
  if (Nfactors == 1) {
    loadingsNOROT <- eigenvectors[,1:Nfactors] * sqrt(eigenvalues[1:Nfactors])
  } else {loadingsNOROT <- eigenvectors[,1:Nfactors] %*% sqrt(diag(eigenvalues[1:Nfactors]))}
  loadingsNOROT <- as.matrix(loadingsNOROT)
  rownames(loadingsNOROT) <- cnoms
  colnames(loadingsNOROT) <- c(paste('Factor ', 1:Nfactors, sep=''))
  
  
  cormat_reprod <- loadingsNOROT %*% t(loadingsNOROT); diag(cormat_reprod) <- 1
  
  
  # communalities
  communalities <- as.matrix(diag(loadingsNOROT %*% t(loadingsNOROT))) 
  communalities <- cbind(rep(1,length(communalities)), communalities)   # as.matrix(communalities) 
  rownames(communalities) <- cnoms
  colnames(communalities) <- c('Initial', 'Extraction')
  uniquenesses <- 1 - communalities
  
  
  fit_coefs <- FIT_COEFS(cormat=cormat, loadings=loadingsNOROT, 
                         extraction='PCA', Ncases=Ncases, verbose=FALSE) 
  
  
  varexplROT <- loadingsROT <- structure <- pattern <- phi <- NA
  
  if (Nfactors > 1) {
    
    # orthogonal rotations
    
    if (rotation == 'varimax') 
      loadingsROT <- VARIMAX(loadingsNOROT, verbose=FALSE)$loadingsV
    
    if (rotation == 'quartimax') 
      loadingsROT <- GPArotation::quartimax(loadingsNOROT)$loadings
    
    if (rotation == 'bentlerT') 
      loadingsROT <- GPArotation::bentlerT(loadingsNOROT)$loadings
    
    if (rotation == 'equamax') 
      loadingsROT <- psych::equamax(loadingsNOROT)$loadings
    
    if (rotation == 'geominT') 
      loadingsROT <- GPArotation::geominT(loadingsNOROT)$loadings
    
    if (rotation == 'entropy') 
      loadingsROT <- GPArotation::entropy(loadingsNOROT)$loadings
    
    # GPArotation::parsimax(loadings)   not an exported object from 'namespace:GPArotation'
    
    
    # oblique rotations
    
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
    
    if (rotation == 'simplimax') {
      outp <- GPArotation::simplimax(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'bentlerQ') {
      outp <- GPArotation::bentlerQ(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    if (rotation == 'geominQ') {
      outp <- GPArotation::geominQ(loadingsNOROT)
      pattern <- outp$loadings
      phi <- outp$Phi
      structure <- pattern %*% phi
    }
    
    
    if (!anyNA(loadingsROT)) varexplROT <- VarianceExplained(eigenvalues, 
                                                                  loadingsROT=loadingsROT)
    
    if (!anyNA(structure)) {			
      colnames(structure) <- colnames(phi) <- c(paste('Factor ', 1:Nfactors, sep=''))
      rownames(phi) <- c(paste('Factor ', 1:Nfactors, sep=''))		
      varexplROT <- VarianceExplained(eigenvalues, loadingsROT=pattern)
    }
  }
  
  pcaOutput <- list(loadingsNOROT = loadingsNOROT,
                    loadingsROT = loadingsROT,
                    pattern = pattern,
                    structure = structure, 
                    phi = phi,
                    varexplNOROT = varexplNOROT,
                    varexplROT = varexplROT,
                    evals12ratio = evals12ratio,
                    cormat_reprod = cormat_reprod,
                    fit_coefs = fit_coefs,
                    communalities = communalities,
                    uniquenesses = uniquenesses)
  
  
  if (verbose ) {
    cat('\n\n\nPrincipal Components Analysis')
    
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    
    if (NfactorsWasNull ) {
      cat('\n\nNfactors was not specified and so the EMPKC test was conducted to determine')
      cat('\nthe number of factors to extraction: Nfactors = ', Nfactors,'\n')		
    } else if (!NfactorsWasNull) {
      cat('\n\nThe specified number of factors to extraction = ', Nfactors,'\n')
    }
    
    cat('\n\nCommunalities:\n\n')
    print(round(communalities,2))
    
    cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
    print(round(varexplNOROT,2), print.gap=4)
    
    cat('\nRatio of the 1st to the 2nd initial eigenvalues = ', round(evals12ratio,1))
    
    cat('\n\n\nModel Fit:\n')
    cat('\n    RMSR = ', round(fit_coefs$RMSR,3))
    cat('\n    GFI = ',  round(fit_coefs$GFI,3))
    cat('\n    CAF = ',  round(fit_coefs$CAF,3))
    
    cat('\n\n\nUnrotated PCA Loadings:\n\n')
    print(round(pcaOutput$loadingsNOROT[,1:Nfactors],2), print.gap=3)
    
    if (Nfactors == 1)  cat('\n\nNo rotation because there is only one component\n')
    
    if (Nfactors > 1) {
      
      if (rotation == 'none')   cat('\n\nRotation procedure:  No rotation')
      
      if (rotation != 'none') {
        
        cat('\n\nThe specified rotation procedure: ', rotation)	
        
        if (!anyNA(loadingsROT)) {
          cat('\n\n\nRotated Loadings:\n\n')	
          print(round(pcaOutput$loadingsROT,2), print.gap=3) 
        }
        
        if (!anyNA(pattern)) { 
          
          cat('\n\n\nPattern Matrix (standardized factor loadings):\n\n');      
          print(round(pcaOutput$pattern,2), print.gap=3)
          
          cat('\n\nStructure Matrix:\n\n');    
          print(round(pcaOutput$structure,2), print.gap=3)
          
          cat('\n\nFactor Correlations:\n\n'); 
          print(round(pcaOutput$phi,2), print.gap=3)
        }
        
        cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
        cat('                    Initial                   Rotated\n')  
        print(pcaOutput$varexplROT, print.gap=4)
      }
    }
   }
  
  return(invisible(pcaOutput))
  
}

