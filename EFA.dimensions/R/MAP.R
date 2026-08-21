



MAP <- function (data, corkind='pearson', Ncases=NULL, verbose=TRUE) {
  # Velicer's MAP test -- takes raw data or a correlation matrix
  
  data <- as.matrix(MISSING_DROP(data))
  
  Nvars  <- ncol(data)
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  datakind <- cordat$datakind
  
  eigs         <- eigen(as.matrix(cormat))
  eigenvalues  <- eigs$values
  eigenvectors <- eigs$vectors
  
  totvarexplNOROT <- VarianceExplained(eigenvalues)
  
  loadings <- eigenvectors %*% sqrt(diag(eigenvalues))
  
  m_values  <- matrix(NA,Nvars,3)
  m_values[,1]  <- 0:(Nvars-1)
  m_values[1,2] <- (sum(sum(cormat^2))-Nvars) / (Nvars*(Nvars-1))
  # m_values[1,3] <- (sum(sum(cormat^4))-Nvars) / (Nvars*(Nvars-1))  WRONG
  M2 <- cormat %*% cormat
  M4 <- M2 %*% M2
  TR4 <- (sum(diag(M4)) - Nvars) / (Nvars * (Nvars - 1))
  m_values[1,3] <- TR4
  
  
  # pb <- utils::txtProgressBar(min = 0, max = (Nvars - 1), style = 3) # create progress bar
  for (m in 1:(Nvars - 1)) {
    #     Sys.sleep(0.1) # for the progress bar
    a <- loadings[,1:m]
    partcov <- as.matrix(cormat - tcrossprod(a,a))  # faster than as.matrix(cormat - (a %*% t(a)))
    
    if (max(partcov) > .0001) {
      d <- diag ( (1 / sqrt(diag(partcov))))
      pr <- d %*% (partcov %*% d)  # faster than d %*% partcov %*% d
      
      # the original, 1976 m values
      # m_values[m+1,2] <- (sum(sum(pr^2))-Nvars) / (Nvars*(Nvars-1))
      
      # alternative computation of the original, 1976 m values, using matrix power
      M2 <- pr %*% pr
      TR2 <- (sum(diag(M2)) - Nvars) / (Nvars * (Nvars - 1))
      m_values[m+1,2] <- TR2
      
      # the 2000, 4rth power m values
      # # my previous, elementwise power approach = not correct
      # m_values[m+1,3] <- (sum(sum(pr^4))-Nvars) / (Nvars*(Nvars-1))
      
      # the matrix power approach to the 2000, 4rth power m values (thanks to Markus Steiner)
      M4 <- M2 %*% M2
      TR4 <- (sum(diag(M4)) - Nvars) / (Nvars * (Nvars - 1))
      m_values[m+1,3] <- TR4
      
    }	else {break}	
    
    #	rm(a,partcov,d,pr) # remove large matrices to free up memory re: R creates duplicates
    #     utils::setTxtProgressBar(pb, m) # update progress bar
  }
  # close(pb)
  
  
  # identifying the smallest fm values & their locations
  NfactorsMAP   <- which.min(na.omit(m_values[,2])) - 1
  NfactorsMAP4  <- which.min(na.omit(m_values[,3])) - 1
  
  dimnames(m_values) <-list(rep('', dim(m_values)[1]))
  colnames(m_values) <- c('root','partial_r_squared','partial_r_4rth_power')
  colnames(m_values) <- c('root','m_pr_squared','m_pr_4rth_power')
  
  
  if (verbose ) { 
    
    cat("\n\nMINIMUM AVERAGE PARTIAL (MAP) TEST")
    
    if (datakind == 'correlations') cat('\n\nThe entered data is a correlation matrix.') 
    
    cat('\n\nNumber of cases = ', Ncases)
    
    cat('\n\nNumber of variables = ', Nvars)
    
    cat('\n\nSpecified kind of correlations for this analysis: ', ctype)
    
    cat('\n\n\nEigenvalues and Proportions of Total Variance Explained:\n\n')
    print(round(totvarexplNOROT,2), print.gap=4)
    
    cat("\n\nVelicer's m values\n\n")
    print(round(m_values,5), print.gap=3)
    
    cat('\nThe smallest partial_r_squared m value is ', 
            round(min(na.omit(m_values[,2])),5))
    cat('\n\nThe smallest partial_r_4rth_power m value is ', 
            round(min(na.omit(m_values[,3])),5))
    
    cat('\n\nThe number of components according to the original (1976) MAP Test is = ', 
            NfactorsMAP,  labels = NULL)
    cat('\n\nThe number of components according to the revised (2000) MAP Test is = ', 
            NfactorsMAP4, labels = NULL, '\n')
  }
  
  mapOutput <- list(totvarexplNOROT=totvarexplNOROT, m_values=m_values, 
                    NfactorsMAP=NfactorsMAP, NfactorsMAP4=NfactorsMAP4)
  
  return(invisible(mapOutput))
  
  cat('\n')
}


