#' @useDynLib fmds, .registration = TRUE
out.of.sample <- function( z, delta )
{
  n <- nrow( z )
  p <- ncol( z )
  if ( !is.vector( delta ) ) stop( "delta must be a vector" )
  if ( length( delta ) != n ) stop( "delta must have appropriate length" )
  if ( anyNA( delta ) ) stop( "NA values not allowed in delta" )
  w <- rep( 1, n )
  x <- rep( 0, p )
  d <- rep( 0, n )
  level <- 0
  MAXITER = 1024
  FCRIT = 0.00000001
  fvalue = 0.0
  echo <- FALSE
  result <- ( .C( "CRpredict", n=as.integer(n), p=as.integer(p), z=as.double(z), delta=as.double(delta), w=as.double(w), x=as.double(x), d=as.double(d), level=as.integer(level), MAXITER=as.integer(MAXITER), FCRIT=as.double(FCRIT), fvalue=as.double(fvalue), echo=as.integer( echo ), PACKAGE = "fmds" ) )
  return( result$x )
} # out.of.sample
