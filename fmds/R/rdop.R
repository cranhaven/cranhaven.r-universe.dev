#' Relative Density-based Outlier Probabilities Function
#'
#' \code{rdop} returns the relative density-based outlier probabilities according to Barroso and Busing (2025).
#'
#' @param data a (rectangular, multivariate, n by m) data matrix or a (n by n ) distance matrix,
#'             in either case, the function continues with a full distance matrix
#' @param k number of neighbors (default: sqrt( 2n ))
#' @param lambda multiple of standard distance deviations to get probabilistic distances
#' @param extended extended relative density-based probabilities
#' @param alpha steepness parameter turning scores into weights
#' @param beta halfway parameter turning scores into weights
#' @return if ( extended == FALSE ): outlier scores; else: weights matrix
#' @references Barroso and Busing (2025).
#'
#' @examples
#' data <- as.matrix( iris[,1:4] )
#' distances <- dist( data )
#' scores <- rdop( data = data )
#' weights <- rdop( distances, extended = TRUE )
#'
#' @author Frank M.T.A. Busing
#'
#' @importFrom stats dist
#' @export
#' @useDynLib fmds, .registration = TRUE
rdop <- function( data, k = 0, lambda = 3.0, extended = FALSE, alpha = 0.20, beta = 0.25 )
{
  if ( inherits( data, "dist" ) ) data <- as.matrix( data )
  if ( !is.matrix( data ) ) stop( "data is not a matrix" )
  if ( !is.numeric( data ) ) stop( "data is not numeric" )
  if ( any( is.na( data ) ) ) stop( "NA's in data not allowed" )

  if ( nrow( data ) != ncol( data ) ) delta <- as.matrix( dist( data ) )
  else if ( !all( diag( data ) == 0.0 ) ) delta <- as.matrix( dist( data ) )
  else if ( any( t( data ) != data ) ) delta <- as.matrix( dist( data ) )
  else delta <- as.matrix( data )

  n <- nrow( delta )
  if ( k <= 0 ) k = as.integer( ifelse( extended == FALSE, sqrt( 2 * n ), n - 1 ) )
  if ( extended == FALSE ) {
    scores <- rep( 0.0, n )
    result <- ( .C( "Crdop", n=as.integer(n), delta=as.double(delta), k=as.integer(k), lambda=as.double(lambda), scores=as.double(scores), PACKAGE = "fmds" ) )
    return( result$scores )
  }
  else {
    w <- matrix( 0.0, n, n )
    result <- ( .C( "Cerdop", n=as.integer(n), delta=as.double(delta), k=as.integer(k), lambda=as.double(lambda), w=as.double(w), alpha=as.double(alpha), beta=as.double(beta), PACKAGE = "fmds" ) )
    return( matrix( result$w, n, n ) )
  }
} # rdop
