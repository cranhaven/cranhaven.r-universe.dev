




REVERSE_CODE <- function(item, max_value = NULL) {
  
  if (is.null(max_value))  max_value <- max(item)
  
  item_recoded <- (min(item) + max(item)) - item
  
  # item_recoded <- max_value + 1 - item
  
  return(item_recoded)	
}



Cronbach.alpha <- function(data) {
  
  data <- MISSING_DROP(data)
  
  # Reliability -- see esp the Footnote, at the bottom.pdf
  itemSDs <- apply(data, 2, sd)
  
  totSD <- sd(rowSums(data))
  
  Nitems <- ncol(data)
  
  Calpha <- (Nitems / (Nitems - 1)) * (1 - ( sum(itemSDs**2) / totSD**2))
  
  cormat <- cor(data)
  
  Calpha.z <- (1 - Nitems / sum(cormat)) * (Nitems / (Nitems - 1))
  
  r_mean <- mean(cormat[lower.tri(cormat)])
  
  r_median <- median(cormat[lower.tri(cormat)])
  
  output <- cbind(Calpha, Calpha.z, r_mean, r_median)
  
  return(invisible(output))
}



# standardized alpha from a correlation matrix
Cronbach.alpha.z <- function(cormat) {
  k <- nrow(cormat)
  k / (k - 1) * (1 - sum(diag(cormat)) / sum(cormat))
}




reproduced_R <- function(loadings) {
  # reproduced correlation matrix from the factor model
  # 1. Compute communalities (sum of squared loadings per row)
  communalities <- rowSums(loadings^2)
  # 2. Compute uniquenesses
  uniquenesses <- 1 - communalities
  # 3. Create Lambda * Lambda Transpose
  Lambda_Lambda_T <- loadings %*% t(loadings)
  # 4. Create the Uniqueness diagonal matrix
  Theta <- diag(uniquenesses)
  # 5. Compute the reproduced correlation matrix
  # cormat_reproduced <- Lambda_Lambda_T + Theta
  Lambda_Lambda_T + Theta
  
  # cormat_reproduced <- loadings_SL %*% t(loadings_SL)
}



RMSR_boc <- function(cormat, cormat_reproduced) {
  residuals <- cormat - cormat_reproduced 
  residuals.upper <- as.matrix(residuals[upper.tri(residuals, diag = FALSE)])
  sqrt(mean(residuals.upper^2)) 
}




# eigenvalues for PCA, PAF, & image -- used by PARALLEL & RAWPAR
eigvals <- function(cormatrix, extraction) {
  
  if (extraction=='PCA')  evals <- eigen(cormatrix)$values
  
  if (extraction=='PAF') {
    smc <- 1 - (1 / diag(solve(cormatrix)))
    diag(cormatrix) <- smc
    evals <- eigen(cormatrix)$values 
  }
  
  if (extraction=='image') { # Gorsuch 1983, p 113; Velicer 1974, EPM, 34, 564
    d <-  diag(1 / diag(solve(cormatrix)))
    gvv <- cormatrix + d %*% solve(cormatrix) %*% d - 2 * d
    s <- sqrt(d)                  #  Velicer 1974 p 565 formula (7)
    r2 <- solve(s) %*%  gvv  %*% solve(s)  # Velicer 1974 p 565 formula (5)
    evals <- cbind(eigen(r2)$values) 
  }
  
  return(invisible(evals))
}





MISSING_DROP <- function(data) {
  
  if (anyNA(data) ) {
    
    # total # of NAs
    totNAs <- sum(is.na(data))
    
    # number of rows (cases) with an NA
    nrowsNAs <- sum(apply(data, 1, anyNA))
    
    data <- na.omit(data)
    message('\nCases with missing values were found and removed from the data matrix.')
    
    message('\nThere were ', nrowsNAs, ' cases with missing values, and ', 
            totNAs, ' missing values in total.\n')		
  }
  return(invisible(data))
}




# set up cormat
setupcormat <- function(data, corkind='pearson', Ncases=NULL) {
  
  # determine whether data is a correlation matrix   
  # there is also a helper function in EFAtools = .is_cormat
  if (nrow(data) == ncol(data)) {
    if (all(diag(data==1))) {datakind = 'correlations'}} else{ datakind = 'notcorrels'}
  if (datakind == 'correlations')  {
    cormat <- as.matrix(data)
    ctype <- 'from user'
    if (is.null(Ncases)) message('\nNcases must be provided when data is a correlation matrix.\n')
  }
  
  if (datakind == 'notcorrels') {
    
    Ncases <- nrow(data)
    
    if (anyNA(data) ) {
      data <- na.omit(data)
      message('\nCases with missing values were found and removed from the data matrix.\n')
    }
    
    if (corkind=='pearson')     {cormat <- cor(data, method='pearson');  ctype <- 'Pearson'}
    if (corkind=='kendall')     {cormat <- cor(data, method='kendall');  ctype <- 'Kendall'}
    if (corkind=='spearman')    {cormat <- cor(data, method='spearman'); ctype <- 'Spearman'} 
    
    # are the data whole numbers?
    if (all((data - round(data)) == 0)) { wholenums = 1 } else { wholenums = 0 }
    
    if (corkind=='polychoric')  {
      if (wholenums == 1)  {
        cormat <- POLYCHORIC_R(data, verbose=FALSE)
        ctype <- 'polychoric'
      }
      if (wholenums == 0)  {
        cormat <- cor(data, method='pearson')
        ctype <- 'Pearson'
        message('\nPolychoric correlations were specified but there are values in the')
        message('\nreal data matrix that are not whole numbers. Pearson correlations')
        message('\nwill be used instead.') 
      }
    }
    
    if (corkind=='gamma') {
      if (wholenums == 1)  {
        cormat <- Rgamma(data, verbose=FALSE)
        ctype <- 'Goodman-Kruskal gamma'
      }
      if (wholenums == 0)  {
        cormat <- cor(data, method='pearson')
        ctype <- 'Pearson'
        message('\nGoodman-Kruskal gamma correlations were specified but there are')
        message('\nvalues in the real data matrix that are not whole numbers.')
        message('\nPearson correlations will be used instead.') 
      }
    }
  }
  
  # smooth cormat if it is not positive definite
  eigenvalues <- eigen(cormat)$values
  if (min(eigenvalues) <= 0)  cormat <- psych::cor.smooth(cormat)
  
  Output <- list(cormat=cormat, ctype=ctype, Ncases=Ncases, datakind=datakind) 
  
  return(invisible(Output))
}





# \cr\cr {Thompson, L. A. 2007. R (and S-PLUS) Manual to Accompany Agresti's Categorical Data 
# Analysis (2002) 2nd edition. https://usermanual.wiki/Document/R2020and20SPLUS2020Manual20to20Accompany20Agrestis20Categorical20Data20Analysis.540471458#google_vignette}


# Goodman-Kruskal gamma - Laura Thompson
# https://usermanual.wiki/Document/R2020and20SPLUS2020Manual20to20Accompany20Agrestis20Categorical20Data20Analysis.540471458#google_vignette
# see p 25 of
# *** 2006 Thompson - S-PLUS (and R) Manual to Accompany Agresti's Categorical Data Analysis - Splusdiscrete2

Gamma.f <- function(x)
{
  # x is a matrix of counts.  You can use output of crosstabs or xtabs.
  n <- nrow(x)
  m <- ncol(x)
  res <- numeric((n-1)*(m-1))
  for(i in 1:(n-1)) {
    for(j in 1:(m-1)) res[j+(m-1)*(i-1)] <- x[i,j]*sum(x[(i+1):n,(j+1):m])
  }
  C <- sum(res)
  res <- numeric((n-1)*(m-1))
  iter <- 0
  for(i in 1:(n-1))
    for(j in 2:m) {
      iter <- iter+1; res[iter] <- x[i,j]*sum(x[(i+1):n,1:(j-1)])
    }
  D <- sum(res)
  gamma <- (C-D)/(C+D)
}



Rgamma  <- function (donnes, verbose=TRUE) {
  
  rgamma <- matrix(-9999,ncol(donnes),ncol(donnes))
  
  for (i in 1:(ncol(donnes)-1) ) {
    for (j in (i+1):ncol(donnes) ) {
      
      dat <- donnes[,c(i,j)]
      
      rgamma[i,j] <- rgamma[j,i] <- Gamma.f(table(dat[,1], dat[,2]))
    }
  }
  diag(rgamma) <- 1
  
  if (verbose ) {
    cat("\nGoodman-Kruskal gamma correlations (Thompson, 2006): \n\n" )
    print(round(rgamma,3))
  }
  
  return(invisible(rgamma))
}





VarianceExplained <- function(eigenvalues, loadingsNOROT=NULL, loadingsROT=NULL, phi=NULL) {
  
  outp <- data.frame(matrix(-999, length(eigenvalues), 9))
  cnoms <- c('Eig', 'prop', 'cum')
  
  # no extraction
  propvar <- eigenvalues / length(eigenvalues)
  cumvar  <- cumsum(propvar) 
  totvarexpl_1 <- cbind(eigenvalues, propvar, cumvar) 
  outp[,1:3] <- totvarexpl_1
  
  # NOROT
  if (!is.null(loadingsNOROT)) {
    if (is.null(phi))  sumsqloads <- colSums(loadingsNOROT**2)
    if (!is.null(phi)) sumsqloads <- diag(phi %*% crossprod(loadingsNOROT))
    propvar <- sumsqloads / length(eigenvalues)
    cumvar  <- cumsum(propvar) 
    totvarexpl_2 <- cbind(sumsqloads, propvar, cumvar) 
    outp[1:nrow(totvarexpl_2),4:6] <- totvarexpl_2
    # cnoms <-  c(cnoms, 'SSL_NOROT', 'prop_NOROT', 'cumul_NOROT')
    cnoms <-  c(cnoms, ' Eig', ' prop', ' cum')
  }
  
  # rotated
  if (!is.null(loadingsROT)) {
    if (is.null(phi))  sumsqloads <- colSums(loadingsROT**2)
    if (!is.null(phi)) sumsqloads <- diag(phi %*% crossprod(loadingsROT))
    propvar <- sumsqloads / length(eigenvalues)
    cumvar  <- cumsum(propvar) 
    totvarexpl_3 <- cbind(sumsqloads, propvar, cumvar) 
    outp[1:nrow(totvarexpl_3),7:9] <- totvarexpl_3
    # cnoms <-  c(cnoms, 'SSL_ROT', 'prop_ROT', 'cumul_ROT')
    cnoms <-  c(cnoms, ' Eig ', ' prop ', ' cum ')
  }
  
  # Remove Columns Where All Elements are Equal 
  # outp <- data.frame(outp)
  # outp <- Filter(function(x) length(unique(x)) > 1, outp)
  
  # remove columns that are all -999
  outp2 <- round(outp,2)
  drop_these <- which(colMeans(outp) == -999)
  if (length(drop_these) > 0)  outp2 <- outp2[-drop_these]
  
  # blank-out the -999 values
  outp2[outp2 == -999] <- paste(rep(" ", 2), collapse = "")
  
  colnames(outp2) <-colnames(outp) <-  cnoms
  # colnames(outp2) <- c('SSL','prop.','cum prop', 'SSL','prop.','cum prop')
  rownames(outp2) <- rownames(outp) <- c(paste('Factor ', 1:nrow(outp2), sep=''))

  
  # # Keep columns where NOT all elements are equal to 0
  # df_clean <- outp[ , !sapply(outp, function(x) all(x == -999))]
  
  # cbind(totvarexpl_1, totvarexpl_2, totvarexpl_3)
  # outp[ , colSums(is.na(outp)) != nrow(outp)]
  # outp2 <- as.data.frame(round(outp,2))
  
  # print(outp2, print.gap=3)
  
  # varex_outp <- list(outp=outp, outp2=outp2)
  varex_outp <- outp2
  
  return(invisible(varex_outp))
}





CAF_boc <- function(cormat, cormat_reproduced=NULL) {
  
  # from 2011 Lorenzo-Seva - The Hull method for selecting the number of common factors
  
  if (is.null(cormat_reproduced)) {bigR <- cormat
  } else {bigR <- cormat - cormat_reproduced;  diag(bigR) <- 1}
  
  # smooth bigR if it is not positive definite
  if (any(eigen(bigR, symmetric = TRUE, only.values = TRUE)$values <= 0)) 
    bigR <- suppressWarnings(psych::cor.smooth(bigR))
  
  # overall KMO
  Rinv <- solve(bigR)	
  Rpart <- cov2cor(Rinv)
  cormat_sq <- bigR^2
  Rpart_sq  <- Rpart^2
  KMOnum <- sum(cormat_sq) - sum(diag(cormat_sq))
  KMOdenom <- KMOnum + (sum(Rpart_sq) - sum(diag(Rpart_sq))) 
  KMO <- KMOnum / KMOdenom
  
  CAF <- 1 - KMO
  
  return(CAF)	
}





FIT_COEFS <- function(cormat, loadings, extraction, Ncases, verbose=TRUE) {
  
  cormat_reproduced <- reproduced_R(loadings)
    
  # model statistics, based on Revelle
  # cormat_reproduced <- loadingsNOROT %*% t(loadingsNOROT); diag(cormat_reproduced) <- 1
  # model <- cormat_reproduced
  # #model <- cor.smooth(model)  #this replaces the next few lines with a slightly cleaner approach
  # #r <- cor.smooth(r)  #this makes sure that the correlation is positive semi-definite
  m.inv.r <- try(solve(cormat_reproduced, cormat), silent=TRUE)
  Nvars <- nrow(loadings)
  Nfactors <- ncol(loadings)
  dfMODEL <- Nvars * (Nvars - 1) / 2 - Nvars * Nfactors + (Nfactors * (Nfactors - 1) / 2)
  objective <- sum(diag((m.inv.r))) - log(det(m.inv.r)) - Nvars 
  chisqMODEL <- objective * ((Ncases - 1) - (2 * Nvars + 5) / 6 - (2 * Nfactors) / 3) # from Tucker & from factanal
  if(!is.nan(chisqMODEL)) if (chisqMODEL < 0) {chisqMODEL <- 0}  
  if (dfMODEL > 0) {pvalue <- pchisq(chisqMODEL, dfMODEL, lower.tail = FALSE)} else {pvalue <- NA}
  
  # the null model
  Fnull <- sum(diag((cormat))) - log(det(cormat)) - Nvars  
  chisqNULL <-  Fnull * ((Ncases - 1) - (2 * Nvars + 5) / 6 )
  dfNULL <- Nvars * (Nvars - 1) / 2
  
  
  # RMSR
  RMSR <- RMSR_boc(cormat, cormat_reproduced)
  # residuals <- cormat - cormat_reproduced 
  # residuals.upper <- as.matrix(residuals[upper.tri(residuals, diag = FALSE)])
  # mnsqdresid <- mean(residuals.upper^2) # mean of the off-diagonal squared residuals (as in Waller's MicroFact)
  # RMSR <- sqrt(mean(residuals.upper^2)) # rmr is perhaps the more common term for this stat
  # # no srmsr computation because it requires the SDs for the variables in the matrix
  
  
  # GFI (McDonald, 1999), & was also from Waller's MicroFact: 
  # 1 - mean-squared residual / mean-squared correlation
  residuals <- cormat - cormat_reproduced
  residuals.upper <- as.matrix(residuals[upper.tri(residuals, diag = FALSE)])
  mnsqdresid <- mean(residuals.upper^2) # mean of the off-diagonal squared residuals (as in Waller's MicroFact)
  mnsqdcorrel <- mean(cormat[upper.tri(cormat, diag = FALSE)]^2)
  GFI <- 1 - (mnsqdresid / mnsqdcorrel)
  
  
  # CAF from Lorenzo-Seva, Timmerman, & Kiers (2011)
  CAF <- CAF_boc(cormat, cormat_reproduced=cormat_reproduced)
  

  RMSEA <- TLI<- CFI <- MFI <- BIC <- AIC <- CAIC <- SABIC <- NA
    
  if (!extraction %in% c('PCA', 'pca','IMAGE','image')) {  

    RMSEA <- sqrt(max(((chisqMODEL - dfMODEL) / (Ncases - 1)),0) / dfMODEL)
    
    # TLI - Tucker-Lewis index (Tucker & Lewis, 1973) = 
    # NNFI - nonnormed fit index (Bentler & Bonett, 1980)
    t1 <- chisqNULL / dfNULL - chisqMODEL / dfMODEL
    t2 <- chisqNULL / dfNULL - 1 
    TLI <- 1
    if(t1 < 0 && t2 < 0) {TLI <- 1} else {TLI <- t1/t2} # lavaan   else {TLI <- 1}  
    NNFI <- TLI
    
    CFI <- ((chisqNULL - dfNULL) - (chisqMODEL - dfMODEL)) / (chisqNULL - dfNULL)
    
    # MacDonald & Marsh (1990) MFI = an absolute fit index that does not depend  
    # on comparison with another model  (T&F, 2001, p 700)
    MFI <- exp (-.5 * ( (chisqMODEL - dfMODEL) / Ncases))
    
    BIC <- chisqMODEL - dfMODEL * log(Ncases)
    
    # AIC Akaike Information Criteria (T&F, 2001, p 700)
    # not on a 0-1 scale; & the value/formula varies across software
    AIC <- chisqMODEL - 2 * dfMODEL
    
    # CAIC Consistent Akaike Information Criteria (T&F, 2001, p 700)
    # not on a 0-1 scale; & the value/formula varies across software
    CAIC <- chisqMODEL - (log(Ncases) + 1) * dfMODEL
    
    # SABIC -- Sample-Size Adjusted BIC (degree of parsimony fit index)		
    # Kenny (2020): "Like the BIC, the sample-size adjusted BIC or SABIC places a penalty 
    # for adding parameters based on sample size, but not as high a penalty as the BIC.  
    # Several recent simulation studies (Enders & Tofighi, 2008; Tofighi, & Enders, 2007) 
    SABIC <- chisqMODEL + log((Ncases+2) / 24) * (Nvars * (Nvars+1) / 2 - dfMODEL)
    
    # mirt:   SABIC <- (-2) * logLik + tmp*log((N+2)/24)
  }
  
  fitcoefsOutput <- list(cormat_reproduced = cormat_reproduced, 
                         chisqMODEL = chisqMODEL, dfMODEL = dfMODEL, pvalue = pvalue,
                         chisqNULL = chisqNULL, dfNULL = dfNULL,
                         RMSR=RMSR, GFI=GFI, CAF=CAF,
                         RMSEA=RMSEA, TLI=TLI, CFI=CFI, MFI=MFI, BIC=BIC, AIC=AIC, 
                         CAIC=CAIC, SABIC=SABIC)
  
  if (verbose) {  
    
    cat('\n\n\nFit Coefficients:')
    
    cat('\n\nChi square = ', round(chisqMODEL,2),
            '   df = ', dfMODEL,'    p = ', round(pvalue,5))
    
    cat('\n\nNull Model Chi square = ', round(chisqNULL,2), '   df = ', dfNULL)
    
    cat('\n\nRMSR = ', round(RMSR,2))
    
    cat('\n\nGFI (McDonald) = ', round(GFI,2))
    
    cat('\n\nCAF = ', round(CAF,2))
    
    if (!extraction %in% c('PCA', 'pca','IMAGE','image')) {  
      
      cat('\n\nRMSEA = ', round(RMSEA,3))
      
      cat('\n\nTLI = ', round(TLI,2))
      
      cat('\n\nCFI = ', round(CFI,2))
      
      cat('\n\nMFI = ', round(MFI,2))
      
      cat('\n\nAIC = ', round(AIC,2))
      
      cat('\n\nCAIC = ', round(CAIC,2))
      
      cat('\n\nBIC = ', round(BIC,2))
      
      cat('\n\nSABIC = ', round(SABIC,2))
    }
  }
  
  return(invisible(fitcoefsOutput))    
}     






VARIMAX <- function (loadings, normalize = TRUE, verbose=TRUE) {
  
  # uses the R built-in varimax function & provides additional output
  
  if (is.list(loadings) == 'TRUE')  loadings <- loadings$loadings
  
  if (ncol(loadings) == 1 & verbose==TRUE) {
    message('\nWARNING: There was only one factor. Rotation was not performed.\n')
  }
  
  if (ncol(loadings) > 1) {
    
    vmaxres <- varimax(loadings, normalize=normalize)  # from built-in stats
    
    loadingsV <- vmaxres$loadings[]
    colnames(loadingsV) <-  c(paste('Factor ', 1:ncol(loadingsV), sep=''))
    
    rotmatV <- vmaxres$rotmat
    colnames(rotmatV) <- rownames(rotmatV) <- c(paste('Factor ', 1:ncol(loadingsV), sep=''))
    
    # reproduced correlation matrix
    cormat_reproduced <- loadingsV %*% t(loadingsV); diag(cormat_reproduced) <- 1
    
    
    if (verbose ) {
      
      cat('\n\n\nVarimax Rotated Loadings:\n\n')
      print(round(loadingsV,2), print.gap=3)
      
      cat('\n\nThe rotation matrix:\n\n')
      print(round(rotmatV,2), print.gap=)
    }
  }
  
  varimaxOutput <-  list(loadingsNOROT=loadings, loadingsV=loadingsV, rotmatV=rotmatV, 
                         cormat_reproduced=cormat_reproduced)  
  
  return(invisible(varimaxOutput))
  
}





# Promax rotation

# from stata.com:
# The optional argument specifies the promax power. 
# Values smaller than 4 are recommended, but the choice is yours. Larger promax 
# powers simplify the loadings (generate numbers closer to zero and one) but 
# at the cost of additional correlation between factors. Choosing a value is 
# a matter of trial and error, but most sources find values in excess of 4 
# undesirable in practice. The power must be greater than 1 but is not 
# restricted to integers. 
# Promax rotation is an oblique rotation method that was developed before 
# the "analytical methods" (based on criterion optimization) became computationally 
# feasible. Promax rotation comprises an oblique Procrustean rotation of the 
# original loadings A toward the elementwise #-power of the orthogonal varimax rotation of A. 


PROMAX <- function (loadings, ppower=4, verbose=TRUE) {  
  
  # uses the R built-in promax function & provides additional output
  
  #if (is.list(loadings) == 'TRUE') loadings <- loadings$loadings
  
  if (ncol(loadings) == 1)  {	
    promaxOutput <- list(loadingsNOROT=loadings, pattern=loadings, structure=loadings)
    return(invisible(promaxOutput))
    if (verbose ) message('\nWARNING: There was only one factor. Rotation was not performed.\n')
  }
  
  if (ncol(loadings) > 1) {
    
    # varimax
    vmaxres <- varimax(loadings, normalize=TRUE)   # SPSS normalizes them
    loadingsV <- vmaxres$loadings[]	
    rotmatV <- vmaxres$rotmat
    
    promaxres <- promax(loadingsV, m=ppower)
    
    bigA <- rotmatV %*% promaxres$rotmat
    
    phi  <- solve(t(bigA) %*% bigA)
    colnames(phi) <- rownames(phi) <- c(paste('Factor ', 1:ncol(loadingsV), sep=''))
    
    Pstructure <- promaxres$loadings %*% phi  # promax structure
    Ppattern   <- promaxres$loadings[]  # promax loadings/pattern
    
    # reproduced correlation matrix
    cormat_reproduced <- Pstructure %*% t(Ppattern); diag(cormat_reproduced) <- 1
    
    
    if (verbose ) {
      
      # message('\nUnrotated Loadings:\n')
      # print(round(B,2))
      
      cat('\n\nPromax Rotation Pattern Matrix:\n\n')
      print(round(Ppattern,2), print.gap=3)
      
      cat('\n\nPromax Rotation Structure Matrix:\n\n')
      print(round(Pstructure,2), print.gap=3)
      
      cat('\n\nPromax Rotation Factor Correlations:\n\n')
      print(round(phi,2), print.gap=3)
    }
    
    promaxOutput <- list(loadingsNOROT=loadings, pattern=Ppattern, structure=Pstructure, 
                         phi=phi, cormat_reproduced=cormat_reproduced)
    
  }
  return(invisible(promaxOutput))
}






# Image Factor Extraction (Gorsuch 1983, p 113; Velicer 1974, EPM, 34, 564)

IMAGE_FA <- function (cormat, Nfactors, Ncases) {
  
  smcINITIAL <- 1 - (1 / diag(solve(cormat)))  # initial communalities
  
  eigenvalues <- eigen(cormat)$values
  
  cnoms <- colnames(cormat)
  
  # factor pattern for image analysis Velicer 1974 p 565 formula (2)
  d <-  diag(1 / diag(solve(cormat)))
  gvv <- cormat + d %*% solve(cormat) %*% d - 2 * d
  s <- sqrt(d)                     #  Velicer 1974 p 565 formula (7)
  r2 <- solve(s) %*%  gvv  %*% solve(s)    #  Velicer 1974 p 565 formula (5)
  eigval <- diag(eigen(r2) $values)
  eigvect <- eigen(r2) $vectors
  l <- eigvect[,1:Nfactors]
  dd <- sqrt(eigval[1:Nfactors,1:Nfactors])
  
  loadingsNOROT <- as.matrix(s %*% l %*% dd)      #  Velicer 1974 p 565 formula (2)
  
  communalities <- as.matrix(diag(loadingsNOROT %*% t(loadingsNOROT))) 	
  communalities <- cbind(smcINITIAL, communalities) 
  rownames(communalities) <- cnoms
  colnames(communalities) <- c('Initial', 'Extraction')
  
  imageOutput <- list(loadingsNOROT=loadingsNOROT, communalities=communalities)
  
  return(invisible(imageOutput))
}





# Maximum likelihood factor analysis - using factanal from stats

MAXLIKE_FA <- function (cormat, Nfactors, Ncases) {
  
  smcINITIAL <- 1 - (1 / diag(solve(cormat)))  # initial communalities
  
  eigenvalues <- eigen(cormat)$values
  
  Nvars <- dim(cormat)[2]
  
  cnoms <- colnames(cormat)
  
  # factanal often generates errors
  # the code below uses fa from psych when factanal produces an error
  essaye1 <- try(factanalOutput <- 
                   factanal(covmat = as.matrix(cormat), n.obs = Ncases, factors = Nfactors, 
                            rotation = 'none'), silent=TRUE)
  
  loadingsNOROT <- communalities <- NA	
  
  if (!inherits(essaye1, "try-error")) {
    
    # chisqMODEL <- unname(factanalOutput$STATISTIC)
    
    # dfMODEL <- unname(factanalOutput$dof)
    
    # pvalue <- unname(factanalOutput$PVAL)
    
    loadingsNOROT <- factanalOutput$loadings[1:dim(factanalOutput$loadings)[1],
                                             1:dim(factanalOutput$loadings)[2], drop=FALSE]
    
    # uniquenesses <- factanalOutput$uniquenesses
  }	
  
  if (inherits(essaye1, "try-error")) {
    
    # using fa from psych if factanal produces an error
    essaye2 <- try(faOutput <- fa(cormat, Nfactors, rotate="Promax", fm="mle"), silent=TRUE) 
    
    if (!inherits(essaye2, "try-error")) {	
      
      # chisqMODEL <- (Ncases - 1 - (2 * Nvars  +  5) / 6 - (2 * Nfactors) / 3) * faOutput$criteria[1] # from psych ?fa page
      
      # dfMODEL <- faOutput$dof
      
      # if (dfMODEL > 0) {pvalue <- pchisq(chisqMODEL, dfMODEL, lower.tail = FALSE)} else {pvalue <- NA}
      
      loadingsNOROT <- faOutput$Structure[1:dim(faOutput$Structure)[1],
                                          1:dim(faOutput$Structure)[2], drop=FALSE]
      
      # uniquenesses <- faOutput$uniquenesses
    }
    if (inherits(essaye2, "try-error")) 	
      message('\nerrors are produced when Nfactors = ', Nfactors, '\n')	
  }
  
  # # the null model
  # Fnull <- sum(diag((cormat))) - log(det(cormat)) - Nvars  
  
  # chisqNULL <-  Fnull * ((Ncases - 1) - (2 * Nvars + 5) / 6 )
  
  # dfNULL <- Nvars * (Nvars - 1) / 2
  
  # if there are no errors
  if (!all(is.na(loadingsNOROT))) {				       
    communalities <- as.matrix(diag(loadingsNOROT %*% t(loadingsNOROT))) 	
    communalities <- cbind(smcINITIAL, communalities) 
    rownames(communalities) <- cnoms
    colnames(communalities) <- c('Initial', 'Extraction')
  }
  maxlikeOutput <- list(loadingsNOROT=loadingsNOROT, communalities = communalities)
  
  return(invisible(maxlikeOutput))
}






PA_FA <- function (cormat, Nfactors, Ncases, iterpaf=100) {
  
  # CFA / PAF  (Bernstein p 189; smc = from Bernstein p 104)
  
  Nvars <- dim(cormat)[1]
  
  eigenvalues <- eigen(cormat)$values
  
  cnoms <- colnames(cormat)
  
  converge  <- .001
  rpaf <- as.matrix(cormat)
  smc <- 1 - (1 / diag(solve(rpaf)))
  smcINITIAL <- smc  # initial communalities
  
  for (iter in 1:(iterpaf + 1)) {
    diag(rpaf) <- smc # putting smcs on the main diagonal of r
    eigval <-  diag((eigen(rpaf) $values))
    # substituting zero for negative eigenvalues
    for (luper in 1:nrow(eigval)) { if (eigval[luper,luper] < 0) { eigval[luper,luper] <- 0 }}
    eigvect <- eigen(rpaf) $vectors
    if (Nfactors == 1) {
      loadingsNOROT <- eigvect[,1:Nfactors] * sqrt(eigval[1:Nfactors,1:Nfactors])
      communalities <- loadingsNOROT^2
    }else {
      loadingsNOROT <- eigvect[,1:Nfactors] %*% sqrt(eigval[1:Nfactors,1:Nfactors])
      communalities <- rowSums(loadingsNOROT^2) 
    }
    if (max(max(abs(communalities-smc))) < converge) { break }
    if (max(max(abs(communalities-smc))) >= converge  & iter < iterpaf) { smc <- communalities }
  }
  loadingsNOROT <- as.matrix(loadingsNOROT)
  
  communalities <- as.matrix(diag(loadingsNOROT %*% t(loadingsNOROT))) 	
  communalities <- cbind(smcINITIAL, communalities) 
  rownames(communalities) <- cnoms
  colnames(communalities) <- c('Initial', 'Extraction')
  
  pafOutput <- list(loadingsNOROT=loadingsNOROT, communalities=communalities) 
  
  return(invisible(pafOutput))
}





# Harris, C. W. On factors and factor scores. P 32, 363-379.

# Harris, C. W. (1962). Some Rao-Guttman relationships." Psychometrika, 27,  247-63. 

# HARRIS, CHESTER W. "Canonical Factor Models for the
# Description of Change." Problems in Measuring Change. (Edited by Chester W.
# Harris.) Madison: University of Wisconsin Press, 1963. Chapter 8, pp.
# 138-55. (a) 

# HARRIS, CHESTER W., editor. Problems in Measuring Change. Madison: University of Wisconsin Press, 1963. 259 pp. (b) 

# HARRIS, CHESTER W. "Some Recent Developments in Factor Analysis." Educational and Psychological Measurement 2 4 : 193-206; Summer 1964. 

# HARRIS, CHESTER W., and KAISER, HENRY F. "Oblique Factor Analytic Solutions by Orthogonal Transformations." Psychometrika 29: 347-62; December 1964.

# Guttman, L. (1953). Image theory for the structure of quantitative
# variates. Psychometrika 18, 277-296.




