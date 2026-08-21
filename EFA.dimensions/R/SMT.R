

# Sequential Chi-Square Model Tests

# Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to 
# retain in exploratory factor analysis: A comparison of extraction methods under 
# realistic conditions. Psychological Methods, 24(4), 468-491.

# "The fit of common factor models is often assessed with the likelihood ratio test 
# statistic (Lawley, 1940) using maximum likelihood estimation (ML), which tests 
# whether the model-implied covariance matrix is equal to the population covariance 
# matrix. The associated test statistic asymptotically follows a Chi-Square distribution 
# if the observed variables follow a multivariate normal distribution and other 
# assumptions are met (e.g., Bollen, 1989). This test can be sequentially applied to 
# factor models with increasing numbers of factors, starting with a zero-factor model. 
# If the Chi-Square test statistic is statistically significant (with e.g., p < .05), 
# a model with one additional factor, in this case a unidimensional factor model, is 
# estimated and tested. The procedure continues until a nonsignificant result is 
# obtained, at which point the number of common factors is identified.

# "Simulation studies investigating the performance of sequential Chi-Square model 
# tests (SMT) as an extraction criterion have shown conflicting results. Whereas 
# some studies have shown that SMT has a tendency to overextraction (e.g., Linn, 1968; 
# Ruscio & Roche, 2012; Schonemann & Wang, 1972), others have indicated that the SMT 
# has a tendency to underextraction (e.g., Green et al., 2015; Hakstian et al., 1982; 
# Humphreys & Montanelli, 1975; Zwick & Velicer, 1986). Hayashi, Bentler, and 
# Yuan (2007) demonstrated that overextraction tendencies are due to violations of 
# regularity assumptions if the number of factors for the test exceeds the true 
# number of factors. For example, if a test of three factors is applied to samples 
# from a population with two underlying factors, the likelihood ratio test statistic 
# will no longer follow a Chi-Square distribution. Note that the tests are applied 
# sequentially, so a three-factor test is only employed if the two-factor test was 
# incorrectly significant. Therefore, this violation of regularity assumptions does 
# not decrease the accuracy of SMT, but leads to (further) overextractions if a 
# previous test was erroneously significant. Additionally, this overextraction 
# tendency might be counteracted by the lack of power in simulation studies with 
# smaller sample sizes. The performance of SMT has not yet been assessed for 
# non-normally distributed data or in comparison to most of the other modern techniques 
# presented thus far in a larger simulation design." (p. 475)


SMT <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  # takes raw data or a correlation matrix
  data <- as.matrix(data)
  
  Nvars  <- ncol(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  NfactorsSMT <- NA
  
  eigenvalues <- eigen(cormat)$values
  
  
  # the null model
  Fnull <- sum(diag((cormat))) - log(det(cormat)) - Nvars  
  chisqNULL <-  Fnull * ((Ncases - 1) - (2 * Nvars + 5) / 6 )
  dfNULL <- Nvars * (Nvars - 1) / 2
  if (dfNULL > 0) {pvalue <- pchisq(chisqNULL, dfNULL, lower.tail = FALSE)} else {pvalue <- NA}
  
  if (pvalue > .05) NfactorsSMT <- 0
  
  if (verbose ) cat('\n\n\nSEQUENTIAL CHI-SQUARE MODEL TEST')
    
  pvalues <- c(0, NA, chisqNULL, dfNULL, pvalue)
  if (pvalue < .05) {
    
    for (root in 1:(dim(cormat)[1]-2)) {	
      
      dof <- 0.5 * ((Nvars - root)^2 - Nvars - root) # The degrees of freedom for the model
      if (dof < 1) {
        if (verbose == 'TRUE') {
          cat('\n\nThe degrees of freedom for the model with ',
                  root,' factors is < 1 and so the procedure was stopped.')}
        break
      }
      
      mlOutput <- EFA(cormat, extraction = 'ml', rotation='none', Nfactors=root, Ncases=Ncases, verbose=FALSE)
      
      pvalues <- rbind(pvalues, 
                       cbind(root, eigenvalues[root], mlOutput$fit_coefs$chisqMODEL, 
                             mlOutput$fit_coefs$dfMODEL, 
                             mlOutput$fit_coefs$pvalue), deparse.level=0)
      if (mlOutput$fit_coefs$pvalue > .05) {
        NfactorsSMT <- root
        break
      }	
    }
  }
  rownames(pvalues) <- rep('',dim(pvalues)[1])
  colnames(pvalues) <- c('Nfactors', 'eigenvalue', 'Chi-Square', 'df', 'pvalue')
  
  smtOutput <- list(NfactorsSMT=NfactorsSMT, pvalues=pvalues)
  
  
  if (verbose ) {
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    cat('\n\nThe number of factors according to the sequential chi-square model test= ', NfactorsSMT,'\n\n')
    print(round(pvalues,6), print.gap=4, row.names=FALSE)
  }
  
  return(invisible(smtOutput))
}
