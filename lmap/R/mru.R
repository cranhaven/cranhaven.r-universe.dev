#' Multinomial Restricted MDU
#'
#' The function mru performs multinomial restricted unfolding for a nominal response
#' variable and a set of predictor variables.
#'
#' @param y An N vector of the responses (categorical).
#' @param X An N by P matrix with predictor variables
#' @param S Positive number indicating the dimensionality of teh solution
#' @param start Type of starting values (da: discriminant analysis, random or list with B and V)
#' @param maxiter maximum number of iterations
#' @param dcrit convergence criterion
#' @return Y Matrix Y from input
#' @return Xoriginal Matrix X from input
#' @return X Scaled X matrix
#' @return G class indicator matrix
#' @return ynames class names of response variable
#' @return xnames variable names of the predictors
#' @return mx means of the predictor variables
#' @return sdx standard deviations of the predictor variables
#' @return U coordinate matrix of row objects
#' @return B matrix with regression coefficients
#' @return Class coordinate matrix
#' @return iters number of iterations
#' @return deviance value of the deviance at convergence
#'
#' @examples
#' \dontrun{
#' data(dataExample_mru)
#' y = as.matrix(dataExample_mru[1:20 , 1])
#' X = as.matrix(dataExample_mru[1:20 , 2:6])
#' output = mru(y = y, X = X, S = 2)
#' }
#'
#' @importFrom nnet class.ind
#' @importFrom stats runif
#'
#' @export
mru = function(y, X, S = 2, start = "da", maxiter = 65536, dcrit = 1e-5)
{


  N = nrow(X)
  P = ncol(X)
  G = class.ind(y)
  C = ncol(G)

  Xoriginal = X
  X = as.matrix(scale(X, center = TRUE, scale = TRUE))
  mx = attr(X, "scaled:center")
  sdx = attr(X, "scaled:scale")

  # initialization
  if( start == "random" ) {
    B <- matrix( runif( P * S ), P, S )
    V <- matrix( runif( C * S ), C, S )
  }
  if ( start == "da" ) {
    tGGinv <- solve( t( G ) %*% G )
    U <- t( X ) %*% G %*% tGGinv
    e <- eigen( ( 1 / N ) * t( X ) %*% X )
    Tmp <- e$vectors %*% diag( sqrt( 1 / e$values ) )
    A <- t( U ) %*% Tmp
    s = svd( A, nu = S, nv = S )
    sV <- matrix( s$v, P, S )
    B <- Tmp %*% sV
    V <- tGGinv %*% t( G ) %*% X %*% B
  }
  else if (is.list(start)){
    B = start$B
    V = start$V
  }

  # call C-code
  res = fastmru(G = G, X = X, B = B, V = V, DCRIT = dcrit)

  # make output object
  output = list(
    y = y,
    Xoriginal = Xoriginal,
    G = G,
    X = X,
    ynames = colnames(G),
    xnames = colnames(X),
    mx = mx,
    sdx = sdx,
    U = X %*% res$B,
    B = res$B,
    V = res$V,
    iter = res$iters,
    deviance = res$deviance
  )
  class(output) = "mru"
  return(output)
}
