validate_diag_mat <- function(x){
  if(!isDiagonal(x)) stop("matrix must be diagonal")
}