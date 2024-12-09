#'
#' Fast version of mru. It runs mru without input checks.
#'
#' @param G indicator matrix of the response variable
#' @param X matrix with predictor variables
#' @param B starting values of the regression weights
#' @param V starting values for class locations
#' @param MAXINNER maximum number of iterations in the inner loop
#' @param FCRIT convergence criterion for STRESS in the inner loop
#' @param MAXITER maximum number of iterations in the outer loop
#' @param DCRIT convergence criterion for the deviance
#' @param error.check extensive check validity input parameters (default = FALSE).
#'
#' @return B estimated regression weights
#' @return V estimated class locations
#' @return Lastinner number of iterations in the last call to STRESS
#' @return Lastfdif last difference in STRESS values in the inner loop
#' @return lastouter number of iterations in the outer loop
#' @return lastddif last difference in deviances in outer loop
#' @return deviance obtained deviance
#'
#'
#'
#' @export
#' @useDynLib lmap, .registration=TRUE

fastmru <- function( G = NULL, X = NULL, B = NULL, V = NULL, MAXINNER = 32, FCRIT = 0.001, MAXITER = 65536, DCRIT = 0.000001, error.check = FALSE ){
  # check for input errors
  if ( error.check == TRUE ) {

    # available
    if ( is.null( G ) ) stop( "missing membership data G")
    if ( is.null( X ) ) stop( "missing prediction data X")
    if ( is.null( B ) ) stop( "missing initial values coefficients B")
    if ( is.null( V ) ) stop( "missing initial coordinates V")

    # G
    if ( !is.matrix( G ) ) stop( "G is not a matrix" )
    if ( !is.numeric( G ) ) stop( "G is not numeric" )
    # add G specifics
    n <- nrow( G )
    c <- ncol( G )

    # X
    if ( !is.matrix( X ) ) stop( "X is not a matrix" )
    if ( !is.numeric( X ) ) stop( "X is not numeric" )
    if ( n != nrow( X ) ) stop( "number of rows G and X do not match")
    p <- ncol( X )

    # B
    if ( !is.matrix( B ) ) stop( "B is not a matrix" )
    if ( !is.numeric( B ) ) stop( "B is not numeric" )
    if ( p != nrow( B ) ) stop( "number of rows B do not match number of columns X")
    m <- ncol( B )

    # V
    if ( !is.matrix( V ) ) stop( "V is not a matrix" )
    if ( !is.numeric( V ) ) stop( "V is not numeric" )
    if ( m != ncol( V ) ) stop( "number of columns Y do not match number of columns B")
    if ( c != nrow( V ) ) stop( "number of rows V do not match number of columns G")

    # MAXINNER
    if ( MAXINNER < 0 ) stop( "negative maximum number of inner iterations MAXINNER not allowed")

    # FCRIT
    if ( FCRIT < 0.0 ) stop( "negative unfolding function convergence criterion not allowed" )

    # MAXITER
    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")

    # DCRIT
    if ( DCRIT < 0.0 ) stop( "negative likelihood function convergence criterion not allowed" )
  }

  # initialization
  n <- nrow( G )
  c <- ncol( G )
  p <- ncol( X )
  m <- ncol( B )
  U <- matrix( 0, n, m )
  theta <- matrix( 0, n, c )
  deviance <- 0.0

  # execution
  res <- ( .C( "Cmulnomrowresmduneg",
             n=as.integer(n),
             c=as.integer(c),
             G=as.double(G),
             p=as.integer(p),
             X=as.double(X),
             m=as.integer(m),
             B=as.double(B),
             V=as.double(V),
             U=as.double(U),
             theta=as.double(theta),
             MAXINNER=as.integer(MAXINNER),
             FCRIT=as.double(FCRIT),
             MAXITER=as.integer(MAXITER),
             DCRIT=as.double(DCRIT),
             deviance=as.double(deviance), PACKAGE = "lmap" ) )

  # finalization
  B <- matrix( res$B, p, m )
  V <- matrix( res$V, c, m )

  return( list( B=B,
				V=V,
                lastiter=res$MAXITER,
				lastddif=res$DCRIT,
                deviance=res$deviance ) )

} # fastmru
