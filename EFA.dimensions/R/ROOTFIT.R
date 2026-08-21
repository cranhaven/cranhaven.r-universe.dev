
ROOTFIT <- function (data, corkind='pearson', Ncases=NULL, extraction='PAF', 
                     verbose = 'TRUE', factormodel) {
  
  # deprecated  
  if (!missing(factormodel))  extraction <- factormodel
  
  data <- MISSING_DROP(data)
  
  Nvars <- ncol(data)
  
  # the analyses will be run for 1-factor to (Nvars * .6)-factor solutions
  # because problems tend to occur for very small factors
  # Nroots <- floor(Nvars * .6)
  Nroots <- Nvars
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  if (extraction %in% c('PCA', 'pca','IMAGE','image')) {
      
    fits <- matrix(-9999,Nroots,5)
    
    fits[,1] <- 1:Nroots
    
    evals <- eigen(cormat)$values
    
    if (min(evals) < 0) {
      message('\nThe correlation matrix is not positive definite, which means that')
      message('there are eigenvalues less than zero. Expect problems.')
    }
    
    fits[,2] <- evals[1:Nroots]
    
    for (root in 1:Nroots) {
      
      if (extraction == 'PCA' | extraction == 'pca') 
        Output <- PCA(cormat, corkind=corkind, Nfactors=root, Ncases=Ncases, 
                      rotation='none', verbose=FALSE)
      
      if (extraction == 'IMAGE' | extraction=='image') 
        Output <- EFA(data=cormat, extraction='image', Nfactors=root, Ncases=Ncases, 
                      rotation='none', verbose=FALSE)		
      
      fits[root,3:5] <- unlist(Output$fit_coefs[c('RMSR', 'GFI', 'CAF')])		
    }
    colnames(fits) <- c('Root','Eigenvalue','RMSR','GFI','CAF')
    rownames(fits) <- matrix((''),nrow(fits),1)
  }
  
  
  if (!extraction %in% c('PCA', 'pca','IMAGE','image')) {
      
    fits <- matrix(-9999,Nroots,13)
    
    fits[,1] <- 1:Nroots
    
    evals <- eigen(cormat)$values
    
    if (min(evals) < 0) {
      message('\nThe correlation matrix is not positive definite, which means that')
      message('there are eigenvalues less than zero. Expect problems.')
    }
    
    fits[,2] <- evals[1:Nroots]
    
    for (root in 1:Nroots) { 
      
      dof <- 0.5 * ((Nvars - root)^2 - Nvars - root) # The degrees of freedom for the model
      if (dof < 1) {
        fits <- fits[1:(root-1),]
        if (verbose == 'TRUE') {
          cat('\nThe degrees of freedom for the model with ',
                  root,' factors is < 1 and so the procedure was stopped.')}
        break
      }
      
      essaye <- try(Output <- EFA(data=cormat, extraction=extraction, Nfactors=root, Ncases=Ncases, 
                                  rotation='none', iterpaf=400, verbose=FALSE), silent=TRUE)		                                       
      if (inherits(essaye, "try-error")) {
        message('\nfactanal produced an error when Nfactors = ', root)
        fits <- fits[1:(root-1),]
        break
      }
      
      fits[root,3:13] <- unlist(Output$fit_coefs[c('RMSR', 'GFI', 'CAF','RMSEA','TLI',
                                                   'CFI','MFI','BIC','AIC','CAIC','SABIC')])
    }
    colnames(fits) <- c('Root','Eigenvalue','RMSR','GFI','CAF','RMSEA','TLI','CFI',
                        'MFI','BIC','AIC','CAIC','SABIC')
    rownames(fits) <- matrix((''),nrow(fits),1)	
  }
  
  
  if (verbose == 'TRUE') {
    
    cat('\n\nFit coefficients for N-factor solutions')
    
    cat('\n\nType of correlations used in the analyses: ', ctype)
    
    cat('\n\nExtraction Method: ', extraction)
    
    cat('\n\nThe number of cases = ', Ncases)
    
    cat('\n\nThe number of variables = ', Nvars,'\n\n') 
    
    print(round(fits,3))
  }
  
  return(invisible(fits))
}

