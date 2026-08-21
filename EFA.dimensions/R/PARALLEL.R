
# parallel analysis of eigenvalues (no real data required)
PARALLEL <- function (Nvars=20, Ncases=300, Ndatasets=100, extraction='PCA', 
                      percentile=95, corkind='pearson', verbose=TRUE, factormodel) {
  
  # deprecated  
  if (!missing(factormodel))  extraction <- factormodel
  
  evals <- matrix(0, nrow = Nvars, ncol = Ndatasets)
  # pb <- utils::txtProgressBar(min = 0, max = Ndatasets, style = 3) 
  for (nds in 1:Ndatasets) { 
    # utils::setTxtProgressBar(pb,nds)
    #	message('Progress - Random Dataset: ', nds, 'of ', Ndatasets, '\r'); utils::flush.console()
    randat <- matrix(rnorm(Ncases*Nvars),nrow=Ncases,ncol=Nvars)
    # random data correlation matrix
    if (corkind=='pearson')      R_rand <- cor(randat, method='pearson') 
    if (corkind=='kendall')      R_rand <- cor(randat, method='kendall') 
    if (corkind=='spearman')     R_rand <- cor(randat, method='spearman') 
    
    # random data eigenvalues
    evals[,nds] <- eigvals(cormatrix=R_rand, extraction=extraction) 
  } 
  
  # mean & percentile eigenvalues for each position
  means <- apply(evals, 1, mean) 
  # sorting the eigenvalues for each root
  for (luper in 1:Nvars) { evals[luper,] <- sort(evals[luper,]) }
  percentiles <- as.matrix(evals[,round((percentile*Ndatasets)/100)])
  
  results <- cbind(1:Nvars,means,percentiles)
  rownames(results) <- matrix((''),nrow(results),1)
  colnames(results) <- c('Root', 'Mean', 'Percentile')
  
  if (verbose ) {
    cat('\n\nPARALLEL ANALYSIS')
    # specification notices
    if (corkind=='pearson')    ctype <- 'pearson' 
    if (corkind=='kendall')    ctype <- 'kendall' 
    if (corkind=='spearman')   ctype <- 'spearman' 
    cat('\n\nType of correlations specified for the random data eigenvalues: ', ctype)
    if (extraction=='PCA')    cat('\n\nExtraction Method: Principal Components') 
    if (extraction=='PAF')    cat('\n\nExtraction Method: Common Factor Analysis')
    if (extraction=='image')  cat('\n\nExtraction Method: Image Factor Extraction')
    cat('\n\nVariables = ', Nvars) 
    cat('\n\nCases = ', Ncases) 
    cat('\n\nNdatasets = ', Ndatasets) 
    cat('\n\nPercentile = ', percentile, '\n\n') 
    print(round(results,3), print.gap=3)
  }
  parOutput <- list(eigenvalues=results)
  return(invisible(parOutput))
}
