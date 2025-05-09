#' Rotation
#'
#' \code{rotation} returns rotation matrix,
#' such that || rotation * source - target ||^2_weights is minimal.
#'
#' @param source n x m source matrix
#' @param weights diagonal of weights matrix, size n
#' @param target if NULL: rotate source to principal axes; otherwise: rotate source to n x m target
#' @param error.check extensive check validity input parameters (default = FALSE).
#'
#' @return rotation matrix
#'
#' @references Gower (1968).
#'             Commandeur (1991).
#'
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE

rotation <- function( source, weights = NULL, target = NULL, error.check = FALSE )
{
  if ( error.check == TRUE ) {
    if ( !is.matrix( source ) || !is.numeric( source ) ) stop( "source must be a numerical matrix" )
    if ( !is.null( weights ) ) {
      if ( !is.vector( weights ) || !is.numeric( weights ) ) stop( "weights must be NULL or a numerical vector" )
      if ( nrow( source ) != length( weights ) ) stop( "invalid length of weights" )
    }
    if ( !is.null( target ) ) {
      if ( !is.matrix( target ) || !is.numeric( target ) ) stop( "target must be a numerical matrix" )
      if ( nrow( source ) != nrow( target ) || ncol( source ) != ncol( target ) ) stop( "source and target must have identical dimensions" )
    }
  }

  # rotate source to principal axes
  if ( is.null( target ) ) {
    if ( !is.null( weights ) ) ata <- t( source ) %*% diag( weights ) %*% source
    else ata <- t( source ) %*% source
    e <- eigen( ata )
    r <- e$vectors
  }

  # rotate source to target
  else {
    n <- nrow( target )
    j <- diag( n ) - 1.0 / n;
    b <- j %*% target
    atb <- t( source ) %*% b
    s <- svd( atb )
    r <- s$u %*% t( s$v )
  }

  # return rotation matrix
  return( r )

} # rotation
