
# Standard Error Scree test for number of factors
# Zoski, K., & Jurs, S. (1996). An objective counterpart to the visual scree test
# for factor analysis: the standard error scree test. 
# Educational and Psychological Measurement, 56(3), 443-451.

SESCREE <- function (data, Ncases=NULL, corkind='pearson', verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  eigenvalues <- cbind(eigen(cormat) $values)
  Neigenvalues <- nrow(eigenvalues)
  rootnum <- 1:Neigenvalues
  eigdata <-  cbind(rootnum, eigenvalues)
  NfactorsSESCREE <- 0
  arbiter <- 1 / Neigenvalues
  results <- matrix(1,(Neigenvalues-2),3)
  for (loop in 1:(Neigenvalues-2)) {
    y <- cbind(eigdata[loop:Neigenvalues,2])
    n <- nrow(y)
    x <- cbind(matrix(1,n,1), eigdata[loop:Neigenvalues,1])
    b <- solve(t(x) %*% x) %*% t(x) %*% y
    pred <- x %*% b
    sse <- sum ((y - pred)^2)
    sderest <- sqrt(sse / (n - 2))
    results[loop,] <- cbind(eigenvalues[loop,1], b[2,1], sderest)
    if (sderest > arbiter) NfactorsSESCREE <- NfactorsSESCREE + 1
  }
  
  results2 <- cbind(1:nrow(results),  results) 
  dimnames(results2) <-list(rep('', dim(results2)[1]))
  colnames(results2) <- c('Root', 'Eigenvalue', 'Slope', 'SE Estimate')
  
  if (verbose ) {
    cat('\n\n\nSTANDARD ERROR SCREE TEST:')
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    cat('\n\nThe SE Estimate must be > than ', arbiter, ' for a component to be nontrivial\n\n')
    print(round(results2,2), print.gap=3)
    cat('\nThe number of factors according to the standard error scree test = ', 
            NfactorsSESCREE, '\n')
  }
  
  sescreeOutput <- list(NfactorsSESCREE=NfactorsSESCREE, SESCREEtable=results2)
  
  return(invisible(sescreeOutput))
}
