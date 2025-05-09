#' Classical Multidimensional Scaling Function
#'
#' \code{pcoa} performs classical multidimensional scaling or principal coordinates analysis.
#' The function uses an eigenvalue decomposition on a Gramm matrix.
#' The data are supposed to be distances, but often dissimilarities will do fine.
#' The data matrix contains nonnegative values, is square, symmetric, and hollow.
#' NA's are not allowed.
#' An additive constant may be provided, which is added to the dissimilarities.
#' This constant might be obtained optimally with the function fastaddconst().
#' Error checking focuses on the data requirements.
#'
#' @param delta dissimilarity matrix, non-negative, square, and hollow.
#' @param lower lower triangular part of dissimilarity matrix.
#' @param data multivariate data matrix.
#' @param p dimensionality (default = 2).
#' @param k number of landmark points (default = NULL, i.e., no landmarks).
#' @param ac additive constant (default = 0.0, i.e., no additive constant).
#' An additive constant can be obtained with the function fastaddconst( d ) or can be user specified.
#' @param q matrix with h independent n-sized variables ( nrow( q ) >= p ),
#' specifying the linear restriction z = qb (coordinates = variables times coefficients)
#' @param faster logical indicating faster but less precise procedure
#' @param error.check extensive check validity input parameters (default = FALSE).
#'
#' @return either n by p coordinates matrix (if q == NULL)
#'         or h by p coefficients matrix b (if q != NULL), in which case z = qb
#'
#' @references Young and Householder (1938)
#'             Torgerson (1952, 1958)
#'             Gower (1966)
#'             Carroll, Green, and Carmone (1976)
#'             De Leeuw and Heiser (1982)
#'             Ter Braak (1992)
#'             Borg and Groenen (2005)
#'
#' @examples
#' delta <- as.matrix( eurodist )
#' p <- 2
#' r <- pcoa( delta = delta, p = p, error.check = TRUE )
#' head( r )
#' plot( r )
#'
#' @author Frank M.T.A. Busing
#'
#' @importFrom stats runif
#' @export
#' @useDynLib fmds, .registration = TRUE

pcoa <- function( delta = NULL,
                  lower = NULL,
                  data = NULL,
                  p = 2,
                  k = NULL,
                  ac = 0.0,
                  q = NULL,
                  faster = FALSE,
                  error.check = FALSE )
{
  if ( error.check == TRUE ) validate( delta = delta,
                                       lower = lower,
                                       data = data,
                                       p = p,
                                       r = q,
                                       error.check = error.check )

#  if ( !is.null( delta ) + !is.null( lower ) + !is.null( data ) != 1 ) stop( "invalid delta/lower/data specification" )

  if ( !is.null( delta ) ) {
    delta <- as.matrix( delta )
    n <- nrow( delta )
  }
  else if ( !is.null( lower ) ) {
    lower <- as.vector( lower )
    n <- ceiling( sqrt( 2 * length( lower ) ) )
    delta <- matrix( 0, n, n )
    delta[lower.tri( delta )] <- lower
    delta <- delta + t( delta )
  }
  else if ( !is.null( data ) ) {
    if ( inherits( data, "dist" ) ) stop( "invalid data specification" )
    if ( faster == FALSE ) delta <- as.matrix( dist( data ) )
    n <- nrow( data )
    m <- ncol( data )
    if ( is.null( k ) ) k <- 0
    else if ( k >= 0 ) k <- max( n / 5, m + 1 )
    else if ( k > n ) k <- 0
  }
  else stop( "missing data specification" )

  # initialization
  seed <- as.integer( runif( 1, 1, as.integer( .Machine$integer.max ) ) )

  if ( !is.null( delta ) ) {
    delta <- as.matrix( delta )
    z <- rep( 0, n * p )
    if ( is.null( q ) ) {
      if ( faster == FALSE ) result <- ( .C( "Cpcoa", n=as.integer(n), delta=as.double(delta), p=as.integer(p), ac=as.double(ac), z=as.double(z), PACKAGE = "fmds" ) )
      else result <- ( .C( "Cfastpcoa", n=as.integer(n), delta=as.double(delta), p=as.integer(p), ac=as.double(ac), z=as.double(z), PACKAGE = "fmds" ) )
      return( matrix( result$z, n, p ) )
    }
    else {
      q <- as.matrix( q )
      h <- ncol( q )
      b <- rep( 0, h * p )
      result <- ( .C( "Crespcoa", n=as.integer(n), delta=as.double(delta), h=as.integer(h), q=as.double(q), p=as.integer(p), ac=as.double(ac), b=as.double(b), z=as.double(z), PACKAGE = "fmds" ) )
      return( matrix( result$b, h, p ) )
    }
  }
  else {
    z <- rep( 0, n * p )
    result <- ( .C( "Cfasterpcoa", n=as.integer(n), m=as.integer(m), data=as.double(t(data)), p=as.integer(p), k=as.integer(k), z=as.double(t(z)), seed=as.integer(seed), PACKAGE = "fmds" ) )
    return( matrix( result$z, n, p, byrow = TRUE ) )
  }
} # pcoa
