#' Solves a triangular system with a Kronecker product structure
#'
#' Solves \eqn{kron(A, B) x = y} where \eqn{A} and \eqn{B} are lower triangular
#' matrices.
#'
#' @export
#' 
#' @param A an \eqn{m x n} matrix
#' @param B an \eqn{p x q} matrix
#' @param y an \eqn{mp x s} matrix
#' 
#' @return x
#' 
#' @example examples/kronSolve.R

forwardsolve.kron = function(A, B, y) {
  n = ncol(A)
  p = nrow(B)
  q = ncol(B)
  s = ncol(y)
  
  res = matrix(NA, nrow = n*q, ncol = s)
  
  for(i in 1:s) {
    Y = matrix(y[,i], nrow = p)
    Xp = forwardsolve(B, Y)
    res[,i] = as.numeric(t(forwardsolve(A, t(Xp))))
  }
  
  res
}