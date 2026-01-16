

# This is a function to check if all elements in a given list are MTS (numeric and no NAs)
# and have the same dimensions

# Input parameters
# X: a list of MTS

#--------------------------------------------------------------------------------------

check_mts <- function(X) {
  
  l <- length(X)
  n_cols <- unlist(lapply(X, ncol))
  max_cols <- max(n_cols)
  
  # All elements in the series must be numeric
  
  check_numerics <- lapply(X, is.numeric)
  
  if (sum(unlist(check_numerics)) != l) {
    
    stop('All MTS must be numeric')
    
  }
  
  
  # X must be a list
  
  
  if (!is.list(X)) {
    
    stop('X must be a list')
    
  }
  
  # All elements in the series must be different from NA
  
  check_nas <- lapply(X, function(x) {sum(is.na(x))})
  
  if (sum(unlist(check_nas)) != 0) {
    
    stop('There are some NAs in the dataset')
    
  }
  
  # All MTS must have the same number of dimensions
  
  if (sum(n_cols) != (max_cols * l)) {
    
    stop('All MTS must have the same number of dimensions')
    
  }
    
}