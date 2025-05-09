#' Fast Stress Function
#'
#' \code{faststress} calculates value for normalized stress with different input parameters.
#' Distances d are optimally scaled.
#'
#' @param lower an n x (n - 1 ) / 2 vector containing lower triangular part of dissimilarity matrix.
#' @param delta an n by n square hollow matrix containing dissimilarities.
#' @param data an n by m multivariate data matrix.
#' @param w an identical sized matrix containing non-negative weights (all ones when omitted).
#' @param z n by p matrix with coordinates.
#' @param d distances between the rows of z, an n by n square hollow matrix containing Euclidean distances.
#'
#' @return n.stress normalized stress value
#'
#'
#' @examples
#' n <- 100
#' m <- 10
#' data <- matrix( runif( n * m ), n, m )
#' d <- dist( data )
#' lower <- as.vector( d )
#' delta <- as.matrix( d )
#' p <- 2
#' zinit <- matrix( runif( n * p ), n, p )
#' r <- fastmds( delta = delta, p = p, z = zinit )
#' faststress( data = data, z = r$coordinates )
#' faststress( data = data, d = r$distances )
#' faststress( delta = delta, z = r$coordinates )
#' faststress( data = data, d = r$distances )
#' faststress( lower = lower, z = r$coordinates )
#' faststress( lower = lower, d = r$distances )
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

faststress <- function( lower = NULL, delta = NULL, data = NULL,  # only one of these can be non-null
                        w = NULL,                                 # weights, format matches data format
                        z = NULL,                                 # coordinate matrix
                        d = NULL )                                # distances, format matches data format
{
  stopifnot( !is.null( lower ) || !is.null( delta ) || !is.null( data ) )
  stopifnot( !is.null( z ) || !is.null( d ) )
  stopifnot( is.null( data ) || is.null( w ) )
  if ( !is.null( lower ) ) {
    target <- as.vector( lower )
    if ( is.null( d ) ) d <- as.vector( dist( z ) )
    else if ( is.matrix( d ) ) d <- as.vector( d[lower.tri( d )] )
    else d <- as.vector( d )
    stopifnot( length( target ) == length( d ) )
    if ( !is.null( w ) ) w <- as.vector( w )
    if ( !is.null( w ) ) stopifnot( length( target ) == length( w ) )
  }
  else if ( !is.null( delta ) ) {
    target <- as.matrix( delta )
    if ( is.null( d ) ) d <- as.matrix( dist( z ) )
    else d <- as.matrix( d )
    if ( !is.null( w ) ) w <- as.matrix( w )
  }
  else if ( !is.null( data ) ) {
    if ( is.null( d ) ) {
      target <- as.vector( dist( data ) )
      d <- as.vector( dist( z ) )
    }
    else if ( is.matrix( d ) ) target <- as.matrix( dist( data ) )
    else if ( is.vector( d ) ) target <- as.vector( dist( data ) )
  }

  if ( is.null( w ) ) alpha <- sum( target * d ) / sum( d^2 )
  else alpha <- sum( w * target * d ) / sum( w * d^2 )

  if ( is.null( w ) ) result <- sum( ( target - alpha * d )^2 ) / sum( target^2 )
  else result <- sum( w * ( target - alpha * d )^2 ) / sum( w * target^2 )

  result
} # faststress
