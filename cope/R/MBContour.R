#' Computes Multiplier Bootstrap realizations of the supremum of a Gaussian 
#' field on a contour.
#'
#' @param x x-Coordinates of the grid on which the data is observed.
#' @param y y-Coordinates of the grid on which the data is observed.
#' @param R An array of dimension c(length(x),length(y),n) containing the 
#'          realizations of the field.
#' @param cont The contours of f at value level
#' @param N The number of Bootstrap realizations to produce. Default is 1000.
#' @importFrom stats rnorm
#' @return A vector of length N containing the Bootstrap realizations of the 
#'         supremum. 
MBContour = function(x, y, R, cont, N = 1000){

  if(length(cont)==0) return(rep(-Inf,N))

  cont_x = unlist(sapply(cont, function(q) q$x))
  cont_y = unlist(sapply(cont, function(q) q$y))
  cont = cbind(cont_x,cont_y)
  
  interp_max = function(G){
    G = matrix(G,ncol=length(y))
    max(fields::interp.surface(list(x=x,y=y,z=G),cont), na.rm = TRUE)
  }
  
  n = dim(R)[3]
  g = matrix(rnorm(n*N),n,N)
  apply(abs(matrix(R,ncol=n) %*% g),2,interp_max) / sqrt(n-2)
}