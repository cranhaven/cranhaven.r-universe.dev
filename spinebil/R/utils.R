#' Generate basis vector in direction i in n dimensions (i <= n)
#'
#' @param i selected direction
#' @param n number of dimensions
#' @return basis vector
#' @export
basis_vector <- function(i, n){
  v <- rep(0,n)
  v[i] <- 1
  v
}

#' Generate 2-d basis in directions i, j in n dimensions (i,j <= n)
#'
#' @param i first basis direction
#' @param j second basis direction
#' @param n number of dimensions
#' @return basis matrix
#' @export
basis_matrix <- function(i, j, n){
  matrix(c(basis_vector(i,n), basis_vector(j,n)), ncol=2)
}
