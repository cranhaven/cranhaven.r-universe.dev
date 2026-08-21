
# three methods of assessing the factorability of a correlation matrix or raw data set

FACTORABILITY <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  cnoms <- colnames(data) # get colnames
  
  Nvars  <- ncol(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  # the determinant of the correlation matrix should be > 0.00001 for factorability
  detcor <- det(cormat)
  
  # Bartlett's identity matrix test
  # cortest.bartlett(cormat)  # from the psych package, same results
  # message('\nThe Bartlett test of whether a correlation matrix is significantly different')
  # message('\nfrom an identity matrix (wherein all of the off-diagonal elements are zero).') 
  
  
  chi2 <- (Ncases - 1 - (2 * Nvars + 5) / 6) * -log(detcor) 
  df <- Nvars * (Nvars - 1) / 2
  pvalue <- pchisq(chi2, df, lower.tail=F)
  
  Rinv <- solve(cormat)
  Rpart <- cov2cor(Rinv) 
  cormat_sq <- cormat^2
  Rpart_sq  <- Rpart^2
  
  # overall KMO
  KMOnum <- sum(cormat_sq) - sum(diag(cormat_sq))
  KMOdenom <- KMOnum + (sum(Rpart_sq) - sum(diag(Rpart_sq))) 
  KMO <- KMOnum / KMOdenom
  
  diag(cormat_sq) <- 0
  diag(Rpart_sq)  <- 0
  KMOvars <- colSums(cormat_sq)/(colSums(cormat_sq) + colSums(Rpart_sq))
  KMOvars <- matrix(KMOvars,length(KMOvars),1)
  rownames(KMOvars) <- cnoms
  colnames(KMOvars) <- 'Variable MSA'
  
  if (verbose==TRUE) {
    
    cat('\n\nThree methods of assessing the factorability of a correlation matrix or raw data set:')
    
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    
    cat('\n\nThe determinant of the correlation matrix should be > 0.00001 for factorability.')
    if (detcor >  0.00001) cat('\n\nThe determinant is ',round(detcor,7),
                                   ' which is > 0.00001, indicating factorability.')
    if (detcor <= 0.00001) cat('\n\nThe determinant is ',round(detcor,7),
                                   ' which is NOT > 0.00001, indicating NON factorability.')
    
    cat('\n\nThe Bartlett test of whether a correlation matrix is significantly different')
    cat('\nfrom an identity matrix (wherein all of the off-diagonal elements are zero):')
    cat('\n\nchisq = ',chi2,'    df= ',df,'     p = ',pvalue)
    cat('\n\nA significant difference is required for factorability.')
    
    cat('\n\n\nThe Kaiser-Meyer-Olkin measure of sampling adequacy (MSA):\n\n')
    
    print(round(KMOvars,2))
    
    cat('\n\nThe overall measure of sampling adequacy (MSA) = ',round(KMO,2))
    
    cat('\n\nThe Kaiser & Rice (1974) interpretation guidelines for MSA values:\n')
    cat('
	   KMO >= .9 is marvelous
	   KMO in the .80s is mertitorious
	   KMO in the .70s is middling
	   KMO in the .60s is mediocre
	   KMO in the .50s is miserable
	   KMO < .5 is unacceptable')
    cat('\n\nConsider excluding items with KMO values < .5 and then re-run the FACTORABILITY analyses.\n')
    
    cat('\nThe overall KMO coefficient indicates the proportion of')
    cat('\nvariance in the variables that might be caused by underlying')
    cat('\nfactors. If the variables share common factors, then the')
    cat('\noverall KMO coefficient should be close to 1.0. The overall')
    cat('\nKMO indicates the extent to which there is at least one')
    cat('\nlatent factor underlying the variables. The overall KMO')
    cat('\nindex is considered particularly meaningful when the cases')
    cat('\nto variables ratio is less than 1:5. The KMO coefficient for')
    cat('\na variable is a kind of summary index of how much a')
    cat('\nvariable overlaps with the other variables.\n')
  }
  
  factOutput <- list(chisq=chi2, df=df, pvalue=pvalue, Rimage=Rpart, KMO=KMO, KMOvars=KMOvars)
  
  return(invisible(factOutput))
  
}
