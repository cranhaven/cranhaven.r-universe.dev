#' Mixed Measurement Level Euclidean Distances Function
#'
#' \code{fastmixed} returns Euclidean distances for variables from mixed measurement levels.
#'
#' @param data an n (objects) by m (variables) numerical data matrix .
#' @param level measurement level variables: 1:numerical, 2:ordinal, 3:nominal (default = 1).
#' @param scale boolean specifying scaling of distances such that sum-of-squares are n times n.
#' @param error.check extensive check validity input parameters (default = FALSE).
#'
#' @return 'dist' object with Euclidean distances between objects.
#'
#' @references Busing (2025). 
#'             A Consistent Distance Measure for Mixed Data: 
#'             Bridging the Gap between Euclidean and Chi-Squared Distances.
#'             Manuscript in progress.
#'
#' @examples
#' data <- as.matrix( iris[,1:4] )
#' d.orig <- as.matrix( dist( data ) )
#' d.mixed <- as.matrix( mdist( data ) )
#' all.equal( d.orig, d.mixed )
#'
#' @author Frank M.T.A. Busing
#' @importFrom stats as.dist
#' @export
#' @useDynLib fmds, .registration = TRUE

mdist <- function( data, level = rep( "numeric", ncol( data ) ), scale = FALSE, error.check = FALSE )
{
  if ( is.vector( data ) ) data <- as.matrix( data )
  if ( error.check == TRUE ) {
    if ( !is.numeric( data ) || !is.matrix( data ) ) stop( "'data' must be a numerical vector or matrix" )
    if ( anyNA( data ) || sum( is.nan( data ) ) != 0 ) stop( "NA or NaN values not allowed in 'data'" )
    if ( length( level ) != ncol( data ) ) stop( "'level' length and number of 'data' columns do not coincide" )
    if ( !is.logical( scale ) ) stop( "'scale' must be a logical (TRUE/FALSE)" )
  }
  data <- as.matrix( data )
  n <- nrow( data )
  m <- ncol( data )
  for ( i in 1:m ) level[i] <- match.arg( level[i], c( "numeric", "ordinal", "nominal" ), several.ok = FALSE )
  intlevel <- rep( 0, m )
  intlevel[level == "numeric"] <- 0
  intlevel[level == "ordinal"] <- 1
  intlevel[level == "nominal"] <- 2
  d <- matrix( 0.0, n, n )
  result <- ( .C( "Cmdist", n=as.integer(n), m=as.integer(m), data=as.double(data), level=as.integer(intlevel), scale=as.integer(scale), d=as.double(d), PACKAGE = "fmds" ) )
  return( as.dist( matrix( result$d, n, n ) ) )
} # mdist
