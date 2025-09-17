#' Simulate matrices from matrix normal distributions
#' 
#' Draw random matrices from the matrix normal distribution \deqn{MN(M, U, V)}  
#' Note that an observation, \eqn{X}, from this equation has the following 
#' distribution when vectorized \deqn{vec(X) ~ N(vec(M), kron(V, U) )}
#'
#' @importFrom stats rnorm
#' 
#' @param n Number of random matrices to simulate
#' @param U Covariance matrix defining dependence between rows
#' @param V Covariance matrix defining dependence between columns
#' @param M average value of each entry in the sampled matrices
#'  

rmatnorm = function(n, U, V, M = matrix(0, nrow=nrow(U), ncol=nrow(V))) {
  
  # initialize output
  nsim = n
  n = nrow(U)
  p = nrow(V)
  r = array( dim = c(n, p, nsim) )
  
  # simulate data
  A = chol(U, pivot = T)
  A = t(A[, order(attr(A, 'pivot'))])
  B = chol(V, pivot=T)
  B = B[, order(attr(B, 'pivot'))]
  for(i in 1:nsim)
    r[,,i] = M + A %*% matrix(rnorm(n*p), nrow=n) %*% B
  
  # return a matrix if only one observation, otherwise return array
  if(nsim==1)
    r[,,1]
  else
    r
}