
NEVALSGT1 <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  # Number of eigenvalues > 1
  
  data <- MISSING_DROP(data)
  
  Nvars  <- ncol(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  datakind <- cordat$datakind
  
  eigenvalues <- cbind(eigen(cormat) $values)
  
  totvarexplNOROT <- VarianceExplained(eigenvalues)
  
  NfactorsNEVALSGT1 <- 0
  for (nev in 1:nrow(eigenvalues)) {if (eigenvalues[nev,] > 1) NfactorsNEVALSGT1 <- NfactorsNEVALSGT1 + 1}
  
  if (verbose ) { 
    cat('\n\nNUMBER OF EIGENVALUES > 1')
    
    if (datakind == 'correlations') cat('\n\nThe entered data is a correlation matrix.') 
    
    cat('\n\nNumber of cases in the data file = ', Ncases)
    
    cat('\n\nNumber of variables in the data file = ', Nvars)
    
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype, '\n')
    
    cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
    print(round(totvarexplNOROT,2), print.gap=4)
    
    cat('\nThe number of eigenvalues greater than one = ', NfactorsNEVALSGT1, '\n')
  }
  
  nevalsgt1Output <- list(NfactorsNEVALSGT1=NfactorsNEVALSGT1, totvarexplNOROT=totvarexplNOROT)
  
  return(invisible(nevalsgt1Output))
  
  cat('\n')
}
