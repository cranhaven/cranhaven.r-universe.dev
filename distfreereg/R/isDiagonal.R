isDiagonal <- function(x){
  is.matrix(x) && identical(x, diag(diag(x)))
}
