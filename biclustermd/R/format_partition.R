#' Format a partition matrix
#' 
#' @param P1 A partition matrix.
#' @return A formatted partition matrix.
#' @description Formats a partition matrix so that subsets in a partition will be ordered by the value of the smallest in each subset
# 1. subsets in a partition will be ordered by the value of the smallest in each subset
format_partition <- function(P1) {
  P <- as.matrix(P1)
  
  n_p <- ncol(as.matrix(P))
  m_p <- nrow(as.matrix(P))
  
  P.form <- matrix(0, ncol = n_p, nrow = m_p)
  
  a <- rep(0, n_p)
  
  for (i in 1:n_p) {
    a[i] <- position_finder(P[, i]) 
  }
  
  num_emps <- which(a == 999999)
  
  non_zero_cols <- which(a != 999999)
  a_non_zero <- a[non_zero_cols]
  
  for (i in 1:length(non_zero_cols)) {
    col_ord_ind <- which(rank(a_non_zero) == i)
    
    P.form[, i] <- P[, col_ord_ind]
  }
  
  return(P.form)
}