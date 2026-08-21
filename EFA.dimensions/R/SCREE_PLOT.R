

SCREE_PLOT <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  Nvars <- ncol(data)
  
  # set up cormat
  cordat <- suppressMessages(setupcormat(data, corkind=corkind, Ncases=Ncases))
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  eigenvalues <- eigen(cormat)$values
  roots <- seq(1:Nvars)
  plot(roots, eigenvalues, pch=15, xlab='Root', ylab='Eigenvalue', cex.lab=1.3, col='blue', 
       type='b', main='Scree Plot')
  axis(1, at=roots)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = .5)
  
  
  totvarexplNOROT <- VarianceExplained(eigenvalues)
  
  
  if (verbose ) {
    
    cat('\n\nScree Plot:')
    
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    
    cat('\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
    print(round(totvarexplNOROT,2), print.gap=4)
  }
  
  return(invisible(totvarexplNOROT))
  
}
