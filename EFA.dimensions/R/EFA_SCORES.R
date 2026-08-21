

EFA_SCORES <- function(loadings=NULL, loadings_type='structure', data=NULL, 
                       cormat=NULL, corkind='pearson', 
                       phi=NULL, method = 'Thurstone', verbose = TRUE) {
  
  if (is.null(loadings) & method != 'PCA')
    message('\nloadings are required when method = ', method, '. Expect errors.')
  
  # get the variable names
  noms <- NULL
  if (!is.null(loadings))                 noms <- rownames(loadings)
  if (is.null(noms) & !is.null(data))     noms <- colnames(data)	
  if (is.null(noms) & !is.null(cormat))   noms <- rownames(cormat)
  
  Nvars <- length(noms)		
  
  if (is.null(data)) {
    message('\n\ndata (with variable or item values) was not provided. Factor scores cannot be computed.')
  } else {
    if (is.null(cormat)) {
      data <- MISSING_DROP(data)
      # set up cormat
      cordat <- setupcormat(data, corkind=corkind, Ncases=NULL)
      cormat <- cordat$cormat
    }
  }
  
  if (is.null(cormat))	
    message('\n\ndata and cormat were not provided, limiting the analytic options.')
  
  # Nfactors
  if (!is.null(loadings) & method != 'PCA')  Nfactors <- ncol(loadings)
  if (method == 'PCA')                       Nfactors <- ncol(cormat)
  
  if (is.null(phi)) {  # if NULL, set phi to an identity matrix
    phi <- diag(1,Nfactors,Nfactors)
    rownames(phi) <- colnames(phi) <- c(paste('Factor ', 1:Nfactors, sep=''))
    message('\n\nphi was not provided and so an identity matrix will be created and')
    message('used in the factor score computations. This assumes that the')
    message('correlations between the factors are all zero. A phi matrix should be')
    message('provided if the factors are correlated.')
  }
  
  structure <- pattern <- NULL
  
  if (method != 'PCA') {
    
    if (loadings_type == 'structure') {	
      structure <- loadings	
      pattern <- structure %*% solve(phi)
    }	
    if (loadings_type == 'pattern') {	
      pattern <- loadings
      structure <- pattern %*% phi
    }
  }
  
  
  # factor score coefficients
  
  if (!is.null(loadings)) {
    if (ncol(loadings) == 1)  method <- "Thurstone"
  }
  
  if (method == 'Thurstone' & !is.null(structure)) {
    # Thurstone (1935) least squares regression approach  (Grice Equation 5)
    # uses the original item correlations and the structure coefficients
    FSCoef <- solve(cormat) %*% structure  
  }
  
  if (method == 'Harman' & !is.null(pattern)) {
    # Harman (1976) "Idealized Variables" (Grice Equation 10) - Univocal for orthogonal factors
    FSCoef <- pattern %*% solve(t(pattern) %*% pattern)
  }
  
  if (method == 'Bartlett' & !is.null(pattern)) {
    # Bartlett (1937) (Grice Equation 9) Univocal for orthogonal factors
    Rep_R <- pattern %*% phi %*% t(pattern)
    Unique <- matrix(diag(diag(Rep_R)),ncol = nrow(Rep_R))
    diag(Unique) <- 1 / (1 - diag(Unique))
    FSCoef <- Unique %*% pattern %*% solve((t(pattern) %*% Unique %*% pattern))
  }
  
  if ((method == 'tenBerge' | method == 'tenberge') & !is.null(pattern) & !is.null(cormat)) {
    # ten Berge et al (1999) McDonald (1981) (Grice Equation 8) correlation preserving
    # factor scores with correlations identical to those in phi
    
    # adapted from Grice  https://psychology.okstate.edu/faculty/jgrice/factorscores/
    
    eis <- eigen(phi)$vectors
    eiv <- eigen(phi)$values
    sqeiv <- diag(sqrt(eiv))
    sq_FR <- eis %*% sqeiv %*% t(eis)
    
    eis <- eigen(cormat)$vectors
    eiv <- eigen(cormat)$values
    sqeiv <- diag(1 / sqrt(eiv))
    sq_R <- eis %*% sqeiv %*% t(eis)
    
    L <- pattern %*% sq_FR
    sqmat <- t(L) %*% solve(cormat) %*% L
    eis <- eigen(sqmat)$vectors
    eiv <- eigen(sqmat)$values
    sqeiv <- diag(1 / sqrt(eiv))
    sq_temp <- eis %*% sqeiv %*% t(eis)
    C_d <- sq_R %*% L %*% sq_temp
    
    FSCoef <- sq_R %*% C_d %*% sq_FR
  }	
  
  if (method == 'Anderson' & !is.null(pattern)) {
    
    # Anderson & Rubin (1956; see Gorsuch, 1983, p 265) only appropriate for orthogonal factor scores
    
    # adapted from Grice Equation 7 & Waller/fungible/faScores.R
    Pkf <- pattern  
    U2 <- 1 - diag(Pkf %*% phi %*% t(Pkf))
    U2_inv <- diag(1/U2)
    PURUP <- t(Pkf) %*% U2_inv %*% cormat %*% U2_inv %*% Pkf
    PURUP_eig <- eigen(PURUP)
    PURUP <- PURUP_eig$vectors %*% diag(PURUP_eig$values**-.5) %*% t(PURUP_eig$vectors)
    FSCoef <- U2_inv %*% Pkf %*% PURUP
  }
  
  
  # PCA unrotated PCA scores
  if (method == "PCA" & !is.null(cormat)) {
    eigs <- eigen(cormat)
    FSCoef <- eigs$vectors
    structure <- eigs$vectors[, 1:ncol(cormat)] %*% sqrt(diag(eigs$values[1:ncol(cormat)]))
    rownames(structure) <- noms
    colnames(structure) <- c(paste('Factor ', 1:Nvars, sep=''))
  }	
  
  rownames(FSCoef) <- noms
  colnames(FSCoef) <- c(paste('Factor ', 1:Nfactors, sep=''))
  
  
  # factor scores
  ZScore <- scale(data)
  FactorScores <- ZScore %*% FSCoef
  
  
  # Indeterminacy Indices -- Grice (2001)  https://psychology.okstate.edu/faculty/jgrice/factorscores/
  
  MULTR <- t(structure) %*% FSCoef 
  RSQR <- diag(MULTR)
  MULTR <- sqrt(diag(MULTR))  # Corr(Fhat, F) for common factors  from fungible
  MINCOR <- 2 * MULTR**2 - 1  # Guttman's Indeterminacy Index: Corr(Fi,Fj) from fungible
  Indeterminacy_mat <- cbind(MULTR, RSQR, MINCOR)   # Multiple R, R-Squared, and Minimum Correlation
  
  
  # Validity & Univocality Coefficients
  C <- t(FSCoef) %*% cormat %*% FSCoef
  C <- sqrt(matrix(diag(diag(C)),ncol = nrow(C)))
  UNIVOCALITY <- t(structure) %*% FSCoef %*% solve(C)
  colnames(UNIVOCALITY) <- colnames(phi)
  VALIDITY <- diag(UNIVOCALITY)
  difference <- abs(VALIDITY - MULTR)
  VALIDITY_mat <- cbind(VALIDITY, difference)
  colnames(VALIDITY_mat)[2] <- 'abs(VALIDITY - MULTR)'
  
  
  # Correlational Accuracy
  FactorScore_Correls <- cor(FactorScores)
  
  
  Output <- list(FactorScores = FactorScores,
                 FSCoef = FSCoef,
                 MULTR = MULTR,
                 RSQR = RSQR,
                 MINCOR = MINCOR,
                 VALIDITY =VALIDITY,
                 UNIVOCALITY = UNIVOCALITY,
                 FactorScore_Correls = FactorScore_Correls,
                 phi = phi,
                 pattern = pattern,
                 structure = structure)
  
  
  if (verbose ) {
    
    cat('\n\nThe method for computing the factor score coefficients (W): ', method)
    
    cat('\n\nFactor Score Coefficients (W):\n\n')
    print(round(FSCoef,3))
    
    cat('\n\nFactor Score Indeterminacy Indices:\n\n')
    print(round(Indeterminacy_mat,3), print.gap=4)
    cat("\n(MINCOR is Guttman's indeterminacy index)")
    
    cat('\n\n\nValidity Coefficients:\n')
    cat('\n(correlations between the factor scores & their respective factors)\n\n')
    print(round(VALIDITY_mat,6), print.gap=4)
    
    cat('\n\nFactor Correlations:\n\n')
    print(round(phi,3))
    
    # # Univocality -- not providing because it is not sufficiently clear
    # diag(UNIVOCALITY) <- NA
    # diag(phi)  <- NA
    # cat('\n\nUnivocality (Rows = Factor Scores / Columns = Factors):\n')
    # print(round(UNIVOCALITY,3), print.gap=4)
    # cat('\nDifferences between UNIVOCALITY & the Factor Correlations:\n')
    # cat('(small values indicate that the estimated factor scores are not heavily')
    # cat('contaminated by variance from other factors in the analysis)\n')
    # print(round(abs(UNIVOCALITY - phi),3), print.gap=4)
    
    cat('\n\nCorrelational Accuracy:\n')
    cat('\n(the following correlations between the factor scores should')
    cat('\nbe similar to the above Factor Correlations)\n\n')
    print(round(FactorScore_Correls,3), print.gap=4)
    cat('\n\nDifferences between the factor score correlations & the factor correlations:\n\n')
    print(round(abs(FactorScore_Correls - phi),3), print.gap=4)
  }
  
  return(invisible(Output))
} 



