
POLYCHORIC_R <- function (data, method='Revelle', verbose=TRUE) {
  
  data <- MISSING_DROP(data)
  
  if (!is.integer(data)) {
    if (!all((data - trunc(data)) == 0)) {
      message('\nThe data matrix does not appear to consist of whole numbers and')
      message('is therefore not appropriate for the computation of polychoric correlations.')
      message('\nConsider stopping the program.\n')
    } 
  }
  
  # finding the max data value or # of levels (the max function does not work for factors)
  Nvalues <- apply(data, MARGIN = 2, function(x) max(x, na.rm=TRUE))
  Nvalues <- max(as.numeric(Nvalues))
  
  # use the polychoric function from the psych package (default)
  if (Nvalues < 9 & (is.null(method) | method=='Revelle')) {
    # rpolysR <- suppressWarnings(psych::polychoric(data, smooth=TRUE))
    # rpolys <- rpolysR$rho
    
    tryRevelle <- function(data){
      tryCatch(
        {
          rpolys <- psych::polychoric(data, smooth=TRUE)$rho
          return(rpolys)
        },
        error=function(e) {
          message('An error occurred when using method=="Revelle". method was therefore changed to "Fox"')
          method <<- "Fox"
          # print(e)
          return(method)
        },
        warning=function(w) {
          message('A warning occurred when using method=="Revelle". method was therefore changed to "Fox"')
          method <<- "Fox"
          # print(w)
          return(method)
        }
      )
    }
    
    rpolys <- tryRevelle(data)
    
    if (verbose ) {
      cat('\n\nPolychoric correlations:\n')
      print(rpolys)
    }
  }
  if (max(Nvalues) > 8) 
  {cat('\nUsing the Fox polycor package because the maximum number of item categories is > 8\n')}
  
  
  # use the hetcor function from the polycor package
  if (method=='Fox' | max(Nvalues) > 8) {
    data <- as.data.frame(data) # the data for hetcor must be a dataframe
    rpolysF <- polycor::hetcor(data)
    rpolys <- rpolysF$correlations
    if (verbose ) {
      cat('\nTypes of correlations computed by hetcor:\n')
      rtypes <- rpolysF$type
      colnames(rtypes) <- rownames(rtypes) <- colnames(data)
      print(rtypes)
      cat('\nPolychoric correlations:')
      print(rpolys)
    }
  }
  
  return(invisible(rpolys))
}
