# Multidimensional Scaling Validation Function
#
# \code{validate} checks all arguments of (most) fmds functions for errors.
# Arguments are not adapted for out-of-bounce or reset to defaults.
# Function will be kept up-to-date depending on feedback.
#
# @author Frank M.T.A. Busing

is.scalar <- function( x ) is.atomic( x ) && length( x ) == 1L && !is.logical( x ) && !is.character( x ) && Im( x ) == 0

validate <- function( delta = NULL,
                      lower = NULL,
                      data = NULL,
                      w = NULL,
                      p = NULL,
                      z = NULL,
                      r = NULL,
                      b = NULL,
                      level = NULL,
                      anchor = NULL,
                      degree = NULL,
                      ninner = NULL,
                      knotstype = NULL,
                      iknots = NULL,
                      approach = NULL,
                      lambda = NULL,
                      alpha = NULL,
                      grouped = NULL,
                      NFOLDS = NULL,
                      NREPEATS = NULL,
                      boundary = NULL,
                      interval = NULL,
                      NCYCLES = NULL,
                      MINRATE = NULL,
                      MAXITER = NULL,
                      FCRIT = NULL,
                      ZCRIT = NULL,
                      rotate = NULL,
                      faster = NULL,
                      error.check = NULL,
                      echo = NULL )
{
  if ( !is.null( delta ) ) {
    if ( inherits( delta, "dist" ) ) delta <- as.matrix( delta )
    if ( !is.matrix( delta ) ) stop( "delta is not a matrix" )
    if ( !is.numeric( delta ) ) stop( "delta is not numeric" )
    if ( nrow( delta ) != ncol( delta ) ) stop( "delta is not square" )
    if ( any( diag( delta ) != 0.0 ) ) stop( "delta diagonal is not zero" )
    n <- nrow( delta )
  }

  if ( !is.null( lower ) ) {
    if ( inherits( lower, "dist" ) ) lower <- as.vector( lower )
    if ( !is.numeric( lower ) ) stop( "lower is not numeric" )
    if ( !is.vector( lower ) ) stop( "lower is not a vector" )
    l <- length( lower )
    mid <- sqrt( 2 * l )
    if ( l != floor( mid ) * ceiling( mid ) / 2 ) stop( "invalid length for lower" )
    n <- ceiling( mid )
  }

  if ( !is.null( data ) ) {
    if ( inherits( data, "dist" ) ) stop( "invalid data specification" )
    if ( !is.numeric( data ) ) stop( "data is not numeric" )
    if ( !is.matrix( data ) ) stop( "data is not a matrix" )
    n <- nrow( data )
  }

  if ( !is.null( w ) ) {
    if ( !is.matrix( w ) && !as.vector( w ) ) stop( "w is neither a matrix, nor a vector" )
    if ( !is.numeric( w ) ) stop( "w is not numeric" )
														   
    if ( any( w < 0.0 ) ) stop( "negative w not allowed" )
    if ( is.matrix( w ) ) {
      if ( nrow( w ) != ncol( w ) ) stop( "w is not square" )
      if ( !all( diag( w ) == 0.0 ) ) stop( "w diagonal is not zero" )
      if ( nrow( delta ) != nrow( w ) ) stop( "number of rows of delta and w do not match")
      if ( ncol( delta ) != ncol( w ) ) stop( "number of columns of delta and w do not match")
    }
  }
  else if ( any( is.na( delta ) ) ) stop( "NA's in delta not allowed without w" )

  if ( !is.null( p ) ) {
    if ( !is.scalar( p ) ) stop( "dimensionality p must be a scalar")
    if ( p <= 0 ) stop( "dimensionality p must be greater than 0")
    if ( p >= n ) stop( "dimensionality p must be smaller than number of objects")
  }

  if ( !is.null( z ) ) {
    if ( !is.matrix( z ) ) stop( "z is not a matrix" )
    if ( !is.numeric( z ) ) stop( "z is not numeric" )
    if ( any( is.na( z ) ) ) stop( "NA's not allowed in z" )
    if ( n != nrow( z ) ) stop( "number of rows z do not match number of objects")
    if ( p != ncol( z ) ) stop( "number of columns z do not match dimensionality p")
  }

  if ( !is.null( r ) ) {
    if ( !is.matrix( r ) ) stop( "r is not a matrix" )
    if ( any( is.na( r ) ) ) stop( "NA's not allowed in r" )
    if ( n != nrow( r ) ) stop( "number of rows r do not match number of rows z")
    if ( is.logical( r ) ) {
      if ( p != ncol( r ) ) stop( "number of columns f do not match number of columns z")
      if ( sum( r ) == 0 ) stop( "r contains free elements only" )
      if ( sum( r ) == n * p ) stop( "r contains fixed elements only" )
    }
    else if ( is.numeric( r ) ) {

    }
    else stop( "unknown restriction" )
  }

  if ( !is.null( b ) ) {
    if ( !is.matrix( b ) ) stop( "b is not a matrix" )
    if ( !is.numeric( b ) ) stop( "b must be numeric" )
    if ( any( is.na( b ) ) ) stop( "NA's not allowed in b" )
    if ( nrow( b ) != ncol( q ) ) stop( "number of rows b does not match number of columns q")
    if ( ncol( b ) != p ) stop( "number of columns b does not match dimensionality p")
  }

  if ( !is.null( level ) ) {
    level <- match.arg( level, c( "none", "linear", "power", "box-cox", "spline", "ordinal" ), several.ok = FALSE )
  }

  if ( !is.null( anchor ) ) {
    if ( is.scalar( anchor ) ) {}
    else if ( is.na( anchor ) ) {}
    else stop ( "anchor must either be a scalar or NA, in which case an additive constant will be estimated" )
  }

  if ( !is.null( ninner ) ) {
    if ( !is.scalar( ninner ) ) stop( "ninner is not a scalar" )
    if ( !is.integer( ninner ) ) stop( "ninner is not an integer" )
    if ( ninner < 0 ) stop( "ninner is negative" )
  }

  if ( !is.null( degree ) ) {
    if ( !is.scalar( degree ) ) stop( "degree is not a scalar" )
    if ( !is.integer( degree ) ) stop( "degree is not an integer" )
    if ( degree < 0 ) stop( "degree is negative" )
  }

  if ( !is.null( knotstype ) ) {
    type <- match.arg( knotstype, c( "none", "uniform", "percentile", "midpercentile" ), several.ok = FALSE )
    if ( type == "none" && ninner != 0 ) stop( "unknown knots type for nonzero inner knots" )
  }

  if ( !is.null( iknots ) ) {
    if ( is.numeric( iknots ) ) {
      if ( ninner != length( iknots ) ) stop( "ninner and length of knots do not match" )
    }
  }

  if ( !is.null( approach ) ) {
    if ( !is.scalar( approach ) ) stop( "approach is not a scalar" )
    if ( !is.integer( approach ) ) stop( "approach is not an integer" )
    if ( approach != 1 || approach != 2 ) stop( "approach can only be 1 or 2" )
  }

  if ( !is.null( lambda ) ) {
    if ( !is.scalar( lambda ) ) stop( "lambda is not a scalar" )
    if ( any( lambda < 0.0 ) ) stop( "negative penalty parameters lambda not allowed")
  }

  if ( !is.null( alpha ) ) {
    if ( !is.scalar( alpha ) ) stop( "alpha is not a scalar" )
    if ( alpha < 0.0 || alpha > 1.0 ) stop( "alpha must be between 0.0 and 1.0")
  }

  if ( !is.null( grouped ) ) {
    if ( !is.logical( grouped ) ) stop( "grouped is not a logical" )
  }

  if ( !is.null( NFOLDS ) ) {
    if ( NFOLDS < 2 ) stop( "minimum number of folds equals 2" )
    if ( NFOLDS > n ) stop( "maximum number of folds equals n" )
  }

  if ( !is.null( NREPEATS ) ) {
    if ( NREPEATS < 1 ) stop( "minimum number of repeats equals 1" )
  }

  if ( !is.null( boundary ) ) {
    if ( !is.numeric( boundary ) ) stop( "boundary is not numeric" )
    if ( !is.scalar( boundary ) ) stop( "boundary is not a scalar" )
    if ( boundary < .Machine$double.eps ) stop( "boundary is too small" )
  }

  if ( !is.null( interval ) ) {
    if ( interval == TRUE && is.null( delta ) ) stop( "delta required for interval scaling" )
  }

  if ( !is.null( NCYCLES ) ) {
    if ( !is.scalar( NCYCLES ) ) stop( "NCYCLES is not a scalar" )
    if ( NCYCLES < 0 ) stop( "negative NCYCLES not allowed" )
  }

  if ( !is.null( MINRATE ) ) {
    if ( MINRATE < .Machine$double.eps ) stop( "MINRATE too small (smaller than machine precision)" )
  }

  if ( !is.null( MAXITER ) ) {
    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")
  }

  if ( !is.null( FCRIT ) ) {
    if ( FCRIT < 0.0 ) stop( "negative function convergence criterion FCRIT not allowed" )
  }

  if ( !is.null( ZCRIT ) ) {
    if ( ZCRIT < 0.0 ) stop( "negative coordinates convergence criterion ZCRIT not allowed" )
  }

  if ( !is.null( rotate ) ) {
    if ( !is.logical( rotate ) ) stop( "rotate is not a logical" )
  }

  if ( !is.null( faster ) ) {
    if ( !is.logical( faster ) ) stop( "faster is not a logical" )
  }

  if ( !is.null( error.check ) ) {
    if ( !is.logical( error.check ) ) stop( "error.check is not a logical" )
  }

  if ( !is.null( echo ) ) {
    if ( !is.logical( echo ) ) stop( "echo is not a logical" )
  }

} # validate
