#' Multidimensional External Unfolding Function
#'
#' \code{external} performs multidimensional external unfolding.
#'
#' @param x an n by m rectangular matrix containing dissimilarities or distances.
#' @param w an identical sized matrix containing nonnegative weights (all ones when omitted).
#' @param fixed fixed column coordinates (m x p).
#' @param z null or initial row coordinates (n by p).
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion (default = 0.00000001).
#' @param error.check extensive check validity input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#'
#' @return x original n by m matrix with dissimilarities or distances.
#' @return w original n by m matrix with dissimilarity weights.
#' @return fixed original m x p fixed column coordinates.
#' @return z final n by p matrix with row coordinates.
#' @return d final n by m matrix with distances between rows of z and rows of fixed.
#' @return last.iteration final iteration number.
#' @return last.difference final function difference used for convergence testing.
#' @return mse final mean squared error function value.
#' @return rmse final root mean squared error function value.
#'
#' @references de Leeuw, J., and Heiser, W. J. (1980). Multidimensional scaling with restrictions on the configuration.
#'             In P.R. Krishnaiah (Ed.), Multivariate analysis (Vol. 5, pp. 501–522).
#'             Amsterdam, The Netherlands: North-Holland Publishing Company.
#'
#'             Heiser,W. J. (1987a). Joint ordination of species and sites: The unfolding technique.
#'             In P. Legendre and L. Legendre (Eds.), Developments in numerical ecology (pp. 189–221).
#'             Berlin, Heidelberg: Springer-Verlag.
#'
#'             Busing, F.M.T.A. (2010). Advances in multidimensional unfolding.
#'             Unpublished doctoral dissertation, Leiden University, Leiden, the Netherlands.
#'
#' @examples
#' library( smacof )
#' data( "breakfast" )
#' x <- as.matrix( breakfast )
#' n <- nrow( x )
#' m <- ncol( x )
#' d.col <- as.matrix( dist( t( x ) ) )
#' r <- smacofSym( d.col )
#' print( r$conf )
#' e <- external( x, fixed = r$conf )
#' print( e$z )
#'
#'
#' @import smacof
#' @export
#' @useDynLib fmdu, .registration=TRUE

external <- function( x,
                      w = NULL,
                      fixed = NULL,
                      z = NULL,
                      MAXITER = 1024,
                      FCRIT = 0.00000001,
                      error.check = FALSE,
                      echo = FALSE )
{
  # check for input errors
  if ( error.check == TRUE ) {

    # w
    if ( !is.null( w ) ) {
      if ( !is.numeric( w ) ) stop( "w is not numeric" )
      if ( any( w < 0.0 ) ) stop( "negative w not allowed" )
      if ( is.vector( w ) ) {
        if ( n != length( w ) ) stop( "number of rows delta and w do not match")
      }
      else if ( is.matrix( w ) ) {
        if ( n != nrow( w ) ) stop( "number of rows delta and w do not match")
        if ( m != ncol( w ) ) stop( "number of columns delta and d do not match")
      }
      else stop( "w is neither a vector nor a matrix" )
      if ( any( is.na( w ) ) ) w[is.na( w )] <- 0.0
    }

    # delta
    if ( !is.matrix( x ) ) stop( "x is not a matrix" )
    if ( !is.numeric( x ) ) stop( "x is not numeric" )
    n <- nrow( x )
    m <- ncol( x )
    if ( any( is.na( x ) ) ) {
      if ( is.null( w ) ) {
        w <- matrix( 1, n, m )
        w[is.na( x )] = 0.0
      }
      else w[is.na( x )] = 0.0
    }

    # fixed
    p <- ncol( fixed )
    if ( is.null( fixed ) ) stop( "missing fixed column coordinates")
    if ( !is.matrix( fixed ) ) stop( "fixed is not a matrix" )
    if ( !is.numeric( fixed ) ) stop( "fixed is not numeric" )
    if ( nrow( fixed ) != m ) stop( "number of rows of fixed and columns of delta do not match" )

    # z
    if ( !is.null( z ) ) {
      if ( !is.matrix( z ) ) stop( "z is not a matrix" )
      if ( !is.numeric( z ) ) stop( "z is not numeric" )
      if ( nrow( z ) != n ) stop( "number of rows of z and rows of delta do not match" )
      if ( ncol( z ) != p ) stop( "number of columns of z and fixed do not match" )
    }

    # MAXITER
    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")

    # FCRIT
    if ( FCRIT < 0.0 ) stop( "negative function convergence criterion not allowed" )
  }

  # initialization
  n <- nrow( x )
  m <- ncol( x )
  p <- ncol( fixed )
  if ( is.null( w ) ) w <- matrix( 1.0, n, m )
  if ( is.null( z ) ) z <- matrix( 0.0, n, p )
  d <- matrix( 0, n, m )
  fvalue <- 0.0

  # execution
  result <- ( .C( "Cexternal", n=as.integer(n), m=as.integer(m), x=as.double(x), w=as.double(w), p=as.integer(p), fixed=as.double(fixed), z=as.double(z), d=as.double(d), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer(echo), PACKAGE= "fmdu" ) )

  # finalization
  x <- matrix( result$x, n, m )
  w <- matrix( result$w, n, m )
  z <- matrix( result$z, n, p )
  d <- matrix( result$d, n, m )
  lastiter <- result$MAXITER
  lastdif <- result$FCRIT
  fvalue <- result$fvalue

  r <- list( x = x,
             w = w,
             fixed = fixed,
             z = z,
             d = d,
             last.iteration = lastiter,
             last.difference = lastdif,
             mse = fvalue,
             rmse = sqrt( fvalue),
             call = match.call() )
  class(r) <- "external"
  r

} # external
