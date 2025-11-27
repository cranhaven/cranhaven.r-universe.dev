#' Discrete Lyapunov Equation Solver
#' 
#' Computes the solution of \eqn{AXA^T - X + W = 0} using the Barraud 1977 approach, adapted from Datta 2004.
#'  This implementation is equivalent to the Matlab implementation of dylap.
#'
#' @param A \eqn{n x n} numeric or complex matrix.
#' @param W \eqn{n x n} numeric or complex matrix.
#'
#' @return The solution to the above Lyapunov equation.
#' @export
#'
#' @references
#' \insertRef{barraud_numerical_1977}{netcontrol}
#' 
#' \insertRef{datta_numerical_2004}{netcontrol}
#' 
#' @examples 
#' A = matrix(c(0,-3,-2,2,-2,1,-1,2,-1), 3,3)
#' C = matrix(c(-2,-8,11,2,-6,13,-3,-5,-2), 3,3)
#' X = dlyap(t(A), C)
#' 
#' print(sum(abs(A %*% X %*% t(A) - X + C)))
dlyap <- function(A, W){
  
  W = -W
  Sch_A <-Matrix::Schur(t(A), vectors = TRUE)
  
  block_diag = !is.complex(eigen(t(A))$values)
  # From Datta 2004 pg. 275, "Numerical Solutions and Conditioning of Lyapunov and Sylvester Equations"
  X_prime = matrix(0, dim(A)[[1]], dim(A)[[2]])
  Sch_T = Sch_A$T
  Q = Sch_A$Q
  C = t(Q)%*%W%*%Q
  X = dlyap_internal(Sch_T, Q, C, dim(A)[[1]], block_diag)
  return(X)
  
}
