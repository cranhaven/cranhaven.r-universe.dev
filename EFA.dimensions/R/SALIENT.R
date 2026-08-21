

# The salient loadings criterion for determing the number of factors


SALIENT <- function (data, salvalue=.4, numsals=3, max_cross=NULL, min_eigval=.7, 
                     corkind='pearson', extraction = 'paf', rotation='promax', 
                     loading_mat = 'structure', ppower = 3, iterpaf=100, Ncases=NULL,
                     verbose=TRUE, factormodel, rotate) {
  
  # deprecated  
  if (!missing(factormodel))  extraction <- factormodel
  if (!missing(rotate))       rotation <- rotate
  
  data <- MISSING_DROP(data)
  
  if (is.null(max_cross))  max_cross = 10
  
  
  # set up cormat
  cordat <- setupcormat(data, corkind=corkind, Ncases=Ncases)
  cormat <- cordat$cormat
  ctype  <- cordat$ctype
  Ncases <- cordat$Ncases
  
  
  # nevalsgt1Output <- NEVALSGT1(cormat, Ncases=Ncases, verbose=FALSE)
  
  # totvarexplNOROT <- nevalsgt1Output$totvarexplNOROT
  
  
  if (verbose) {
    cat('\n\n\nNUMBER OF SALIENT LOADINGS:')
    cat('\n\nThe specified kind of correlations for this analysis: ', ctype)
    cat('\n\nThe salient loading value = ', salvalue)
    cat('\n\nThe required number salient loadings = ', numsals)
    if (!is.null(max_cross)) cat('\n\nThe specified maximum value for cross-loadings = ', max_cross)
    cat('\n\nThe minimum eigenvalue for including a factor in the analyses = ', min_eigval)	
    cat('\n\nThe factor extraction method = ', extraction)
    cat('\n\nThe factor rotation method = ', rotation)	
    cat('\n\nStep-wise results for the number of loadings, per factor, that meet the criteria:\n\n')	
  }
  
  
  # determine how many eigenvalues are >= the specified minimum 
  # Nfactors <- sum(totvarexplNOROT[,1] >= min_eigval)
  eigvals <- eigen(cormat)$values
  Nfactors <- sum(eigvals >= min_eigval)
  
  
  NfactorsSALIENT <- 0
  
  for (lupeNF in Nfactors:1) {
    
    if (extraction == 'pca' | extraction == 'PCA')
      outpEFA <- PCA(data, corkind==corkind, Nfactors=lupeNF, Ncases=Ncases, rotation=rotation, 
                     ppower=ppower, verbose=FALSE)
    
    if (extraction != 'pca' & extraction != 'PCA') 		
      outpEFA <- EFA(data, corkind=corkind, extraction=extraction, rotation=rotation, 
                     Nfactors=lupeNF, Ncases=Ncases, iterpaf=iterpaf, ppower=ppower, verbose=FALSE)
    
    if (rotation == 'none') loadings <- outpEFA$loadingsNOROT
    
    if (rotation == 'varimax' | rotation == 'quartimax' | rotation == 'bentlerT' | rotation == 'equamax' |
        rotation == 'geominT' | rotation == 'entropy')  
      loadings <- outpEFA$loadingsROT
    
    if (rotation == 'promax'    | rotation == 'quartimin'| rotation == 'oblimin' | rotation == 'oblimax' |
        rotation == 'simplimax' | rotation == 'bentlerQ' | rotation == 'geominQ' | rotation == 'bifactorQ')  {
      
      if (loading_mat == 'structure') loadings <- outpEFA$structure
      if (loading_mat == 'pattern')   loadings <- outpEFA$pattern
    }
    
    if (lupeNF == 1) loadings <- outpEFA$loadingsNOROT
    
    if (is.null(max_cross))  max_cross = 10
    
    Nsals_cols <- matrix(0, 3, ncol(loadings))
    
    for (lupec in 1:ncol(loadings)) {
      
      getrownums <- which(abs(loadings[,lupec]) >= salvalue[1])
      
      if (lupeNF == 1)  Nsals_cols[1,lupec] <- length(getrownums)
      
      if (length(getrownums) == 1) Nsals_cols[1,lupec] <- 1
      
      if (length(getrownums) > 1 & lupeNF > 1) {
        
        dumred <- loadings[getrownums,-lupec, drop=FALSE]				
        
        Nsals_cols[1,lupec] <- length(which( apply(abs(dumred), 1, FUN = max) <= abs(max_cross)))
      }
      
      if (length(salvalue) > 1) {
        
        getrownums <- which(abs(loadings[,lupec]) >= salvalue[2])
        
        if (lupeNF == 1)  Nsals_cols[2,lupec] <- length(getrownums)
        
        if (length(getrownums) == 1) Nsals_cols[2,lupec] <- 1
        
        if (length(getrownums) > 1 & lupeNF > 1) {
          
          dumred <- loadings[getrownums,-lupec, drop=FALSE]				
          
          Nsals_cols[2,lupec] <- length(which( apply(abs(dumred), 1, FUN = max) <= abs(max_cross)))
        }
      }
      
      if (length(salvalue) > 2) {
        
        getrownums <- which(abs(loadings[,lupec]) >= salvalue[3])
        
        if (lupeNF == 1)  Nsals_cols[3,lupec] <- length(getrownums)
        
        if (length(getrownums) == 1) Nsals_cols[3,lupec] <- 1
        
        if (length(getrownums) > 1 & lupeNF > 1) {
          
          dumred <- loadings[getrownums,-lupec, drop=FALSE]				
          
          Nsals_cols[3,lupec] <- length(which( apply(abs(dumred), 1, FUN = max) <= abs(max_cross)))
        }
        
      }
    }
    
    rownames(Nsals_cols) <- c(paste('N >= abs(', salvalue[1], ')', sep=''),
                              paste('N >= abs(', salvalue[2], ')', sep=''),
                              paste('N >= abs(', salvalue[3], ')', sep=''))
    colnames(Nsals_cols) <- colnames(loadings)
    
    meet1 <- sapply(Nsals_cols[1,], function(x) x > numsals[1])
    meet2 <- sapply(Nsals_cols[2,], function(x) x > numsals[2])
    meet3 <- sapply(Nsals_cols[3,], function(x) x > numsals[3])
    
    meets <- rbind(meet1, meet2, meet3)
    
    # does each factor meet the criteria at least once?
    meetsF <- apply(meets, 2, function(x) any(x) ==TRUE) 	
    
    if (verbose & lupeNF <= 10) { 
      Nsals_cols <- Nsals_cols[1:length(salvalue), ,drop=FALSE]
      print(Nsals_cols); cat('\n')
    }
    
    if (NfactorsSALIENT == 0 & all(meetsF))  {
      NfactorsSALIENT <- ncol(loadings)
      loadings_final <- loadings
      #break
    }		
  }
  
  if (verbose) {
    cat('\nThe number of factors according to the salient loadings criterion = ', NfactorsSALIENT, '\n')
    cat('\n\nThe loading matrix:\n\n')
    print(round(loadings_final,2), print.gap=3)
  }
  
  salientOutput <- list(NfactorsSALIENT=NfactorsSALIENT, loadings_final=loadings_final)
  
  return(invisible(salientOutput))
}


# message('\nThe specified kind of correlations for this analysis: ', appendLF=FALSE); cat(ctype)
# 
# message('\nThe specified kind of correlations for this analysis: ', appendLF=FALSE); cat(ctype)
# 
# message('\n\nThe salient loading value = ', appendLF=FALSE, salvalue)
# 
# message('\n\nThe required number salient loadings = ', appendLF=FALSE); cat(numsals)
# if (!is.null(max_cross)) message('\n\nThe specified maximum value for cross-loadings = ', appendLF=FALSE); cat(max_cross)
# message('\n\nThe minimum eigenvalue for including a factor in the analyses = ', appendLF=FALSE); cat(min_eigval)	
# message('\n\nThe factor extraction method = ', appendLF=FALSE); cat(extraction)
# message('\n\nThe factor rotation method = ', appendLF=FALSE); cat(rotation)	
