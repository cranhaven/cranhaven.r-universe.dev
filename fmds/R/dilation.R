#' Dilation
#'
#' \code{dilation} returns dilation or scale factor,
#' such that || factor * source - target ||^2_weights is minimal.
#'
#' @param source n x m source matrix
#' @param weights weights matrix, size n
#' @param target if NULL: rotate source to principal axes; otherwise: rotate source to n x m target
#' @param error.check extensive check validity input parameters (default = FALSE).
#' @return dilation factor
#' @references Gower (1968).
#'             Commandeur (1991)
#'
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

dilation <- function( source, weights = NULL, target = NULL, error.check = FALSE )
{
  if ( error.check == TRUE ) {
    if ( !is.matrix( source ) || !is.numeric( source ) ) stop( "source must be a numerical matrix" )
    if ( !is.null( weights ) ) {
      if ( !is.matrix( weights ) || !is.numeric( weights ) ) stop( "weights must be NULL or a numerical matrix" )
      if ( nrow( source ) != nrow( weights ) ) stop( "invalid size weights" )
      if ( nrow( source ) != ncol( weights ) ) stop( "invalid size weights" )
    }
    if ( !is.null( target ) ) {
      if ( !is.matrix( target ) || !is.numeric( target ) ) stop( "target must be a numerical matrix" )
      if ( nrow( source ) != nrow( target ) || ncol( source ) != ncol( target ) ) stop( "source and target must have identical dimensions" )
    }
  }

  if ( is.null( weights ) ) weights <- diag( nrow( source ) )

  d <- as.matrix( dist( target ) )
  upper <- sum( weights * d^2 )
  d <- as.matrix( dist( source ) )
  lower <- sum( weights * d^2 )

  alpha <- sqrt( upper / lower )

  # return dilation factor
  return( alpha )
} # dilation
