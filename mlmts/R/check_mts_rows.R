

# This is a function to check if all MTS in the list have the same length

# Input parameters
# X: a list of MTS

#--------------------------------------------------------------------------------------

check_mts_rows <- function(X) {
  
  l <- length(X)
  n_rows <- unlist(lapply(X, nrow))
  max_rows <- max(n_rows)
  
  # All elements in the series must have the same number of rows
  
  if (sum(n_rows) != (max_rows * l)) {
    
    stop('All MTS must have the same length')
    
  }
  

  
  
}