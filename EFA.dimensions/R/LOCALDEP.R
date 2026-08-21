

LOCALDEP <- function (data, corkind='pearson', item_type='graded', 
                      thetas=NULL, theta_type='EAP', verbose=TRUE) {
  
  # eigenvalues & partial correlations, Q3, & other local dep stats 
  # (partial r = after partialling out the IRT latent trait)
  
  data <- MISSING_DROP(data)
  
  Nvars  <- ncol(data)
  
  noms <- colnames(data) 
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  datakind <- cordat$datakind
  
  colnames(cormat) <- rownames(cormat) <- noms
  
  if (datakind == 'correlations') {
    message('\n\nThe analyses cannot proceed because "data" is a correlation matrix.')
    message('Item-level raw data are required.\n')
  }
  
  if (datakind == 'notcorrels') {
    
    eigs         <- eigen(as.matrix(cormat))
    eigenvalues  <- eigs$values
    eigenvectors <- eigs$vectors
    
    totvarexplNOROT <- VarianceExplained(eigenvalues)
    
    
    # partial correlations, Q3, & other local dep stats
    
    mirt_mod <- mirt::mirt(data, item_type=item_type, 1)
    
    if (is.null(thetas))  thetas <- mirt::fscores(mirt_mod, method = theta_type)
    
    data2 <- cbind(data, thetas); colnames(data2)[(Nvars+1)] <- 'thetas'
    
    partialcors <- psych::partial.r(data = data2, x = noms, y = 'thetas')
    
    resid_Q3   <- mirt::residuals(mirt_mod, type = 'Q3',   Theta=thetas, verbose = FALSE)
    resid_LD   <- mirt::residuals(mirt_mod, type = 'LD',   Theta=thetas, verbose = FALSE, df.p = TRUE)
    resid_LDG2 <- mirt::residuals(mirt_mod, type = 'LDG2', Theta=thetas, verbose = FALSE, df.p = TRUE)
    resid_JSI  <- mirt::residuals(mirt_mod, type = 'JSI',  Theta=thetas, verbose = FALSE)
    
    # place the coefs in long format (rather than matrix/square), to be later sorted by magnitude
    long_noms  <- data.frame(matrix(ncol = 2, nrow = 0))
    long_coefs <- data.frame(matrix(ncol = 8, nrow = 0))
    
    for (luper in 1:(Nvars-1)) {
      for (lupec in (luper+1):Nvars) {
        
        long_noms <- rbind(long_noms, data.frame(noms[luper], noms[lupec]))
        
        long_coefs <- rbind(long_coefs, cbind(cormat[lupec,luper], partialcors[lupec,luper], 
                                              resid_Q3[lupec,luper], resid_LD$LD[lupec,luper], resid_LD$df.p[luper,lupec],
                                              resid_LDG2$LDG2[lupec,luper], resid_LDG2$df.p[luper,lupec],
                                              resid_JSI[lupec,luper]))
      }
    }
    colnames(long_noms)  <- c('Item_A', 'Item_B')
    colnames(long_coefs) <- c('r', 'partial_r', 'Q3', 'X2', 'p_X2', 'G2', 'p_G2', 'JSI')
    
    localdep_stats <- data.frame(cbind(long_noms, long_coefs))
    
    
    # # residuals correls after partialling out the first component
    # loadings <- eigenvectors %*% sqrt(diag(eigenvalues))
    # a <- loadings[,1:1]
    # partcov <- as.matrix(cormat - (a %*% t(a)))
    # d <- diag ( (1 / sqrt(diag(partcov))))
    # partialcors <- d %*% partcov %*% d
    # colnames(partialcors) <- noms
    # rownames(partialcors) <- noms
    
    # numbers of residual correlations >= particular values
    totalN <- Np1 <- Np2 <- Np3 <- Np4 <- Np5 <- Np6 <- Np7 <- Np8 <- 0
    for (i in 1:(ncol(partialcors)-1)) {
      for (j in (i+1):ncol(partialcors)) {
        totalN <- totalN + 1; 
        if (partialcors[i,j] >= .1)  Np1 = Np1 + 1
        if (partialcors[i,j] >= .2)  Np2 = Np2 + 1 
        if (partialcors[i,j] >= .3)  Np3 = Np3 + 1 
        if (partialcors[i,j] >= .4)  Np4 = Np4 + 1 
        if (partialcors[i,j] >= .5)  Np5 = Np5 + 1 
        if (partialcors[i,j] >= .6)  Np6 = Np6 + 1 
        if (partialcors[i,j] >= .7)  Np7 = Np7 + 1 
        if (partialcors[i,j] >= .8)  Np8 = Np8 + 1	
      }
    }				
    
    
    if (verbose ) { 
      
      cat('\n\nStatistics for assessing local dependence:\n')
      
      cat('\nNumber of cases = ', Ncases)
      cat('\n\nNumber of variables = ', Nvars)
      
      # cat('\nSummary statistics for the data file:\n')
      # print(summary(data))
      
      cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
      
      cat('\n\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
      print(round(totvarexplNOROT,2), print.gap=4)
      
      cat('\nRatio of the 1st to the 2nd eigenvalue = ', round((eigenvalues[1] / eigenvalues[2]),1))
      
      cat('\n\n\nOriginal correlation matrix:\n\n')
      print(round(cormat,2))
      
      cat('\n\nResidual item correlations after partialling out the theta values:\n\n')
      print(round(partialcors,2))
      
      cat('\n\n The # of residual correlations >= .1 is  ', Np1, '   percentage = ', round((Np1/totalN),2) , 
              '\n The # of residual correlations >= .2 is  ', Np2, '   percentage = ', round((Np2/totalN),2) ,
              '\n The # of residual correlations >= .3 is  ', Np3, '   percentage = ', round((Np3/totalN),2) ,
              '\n The # of residual correlations >= .4 is  ', Np4, '   percentage = ', round((Np4/totalN),2) ,
              '\n The # of residual correlations >= .5 is  ', Np5, '   percentage = ', round((Np5/totalN),2) ,
              '\n The # of residual correlations >= .6 is  ', Np6, '   percentage = ', round((Np6/totalN),2) ,
              '\n The # of residual correlations >= .7 is  ', Np7, '   percentage = ', round((Np7/totalN),2) ,
              '\n The # of residual correlations >= .8 is  ', Np8, '   percentage = ', round((Np8/totalN),2)  )
      
      cat('\n\n\nLocal dependence statistics sorted by the partial (residual) item correlations:\n\n')
      localdep_stats <- data.frame(lapply(localdep_stats, function(y) if(is.numeric(y)) round(y, 3) else y)) 
      print(localdep_stats[order(abs(localdep_stats$partial_r)),], print.gap=4, row.names = FALSE)
      
    }
    
    localdepOutput <- list(correlations=cormat, residcor=partialcors, 
                           eigenvalues=eigenvalues, resid_Q3=resid_Q3, resid_LD=resid_LD,
                           resid_LDG2=resid_LDG2, resid_JSI=resid_JSI, localdep_stats=localdep_stats)
    
    return(invisible(localdepOutput))
  }
  
}
