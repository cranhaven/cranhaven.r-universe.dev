#' Explain method for all fmds objects
#'
#' \code{explain} fits variables to coordinate configuration
#' 
#' @param x fmds object 
#' @param q variables 
#' @param w weights 
#' @param level transformation level
#' @param MAXITER maximum number of iterations
#' @param FCRIT convergence criterion
#' @param error.check error chacking
#' @param echo intermediate output
#' @param \ldots additional arguments to be passed.
#'
#' @return data data
#' @return weights weights
#' @return transformed.data transformed data
#' @return approach approach
#' @return degree zero
#' @return ninner zero
#' @return iknots NULL
#' @return anchor anchor
#' @return knotstype zero
#' @return coordinates NULL
#' @return coefficients coefficients
#' @return distances distances
#' @return last.iteration last.iteration
#' @return last.difference last.difference
#' @return n.stress n.stress
#' @return stress.1 stress.1
#' @return call call
#'
#' @export
#' @useDynLib fmds, .registration = TRUE
explain.fmds <- function( x,
                            q,
                            w = NULL,
                            level = c( "absolute", "ratio", "linear", "ordinal", "nominal" ),
                            MAXITER = 1024,
                            FCRIT = 0.00000001,
                            error.check = FALSE,
                            echo = FALSE,
                            ... )
{
  n <- nrow( x$coordinates )
  p <- ncol( x$coordinates )
  if ( is.null( w ) ) w <- rep( 1, n )

    # check for input errors
  if ( error.check == TRUE ) {

    if ( !is.vector( q ) ) stop( "'q' must be a vector" )
    if ( length( q ) != n ) stop( "'q' must have appropriate length" )
    if ( anyNA( q ) ) stop( "NA values not allowed in 'q'" )

    if ( !is.vector( w ) ) stop( "'w' must be a vector" )
    if ( length( w ) != n ) stop( "'w' must have appropriate length" )
    if ( anyNA( w ) ) stop( "NA values not allowed in 'w'" )

    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")

    if ( FCRIT < 0.0 ) stop( "negative function convergence criterion not allowed" )
  }

  levels <- c( "absolute", "ratio", "linear", "ordinal", "nominal" )
  if ( is.numeric( level ) ) level <- levels[level + 1]
  level <- match.arg( level, levels, several.ok = FALSE )
  if ( level == "absolute" ) level = 0
  else if ( level == "ratio" ) level = 1
  else if ( level == "linear" ) level = 2
  else if ( level == "ordinal" ) level = 3
  else if ( level == "nominal" ) level = 4
  else level = 0

  a <- rep( 0, p )
  e <- rep( 0, n )
  if ( MAXITER < 0 ) MAXITER = 1024
  if ( FCRIT < 0.0 ) FCRIT = 0.00000001
  fvalue = 0.0

  result <- ( .C( "CRexplain", n=as.integer(n),
                               p=as.integer(p),
                               z=as.double(x$coordinates),
                               q=as.double(q),
                               w=as.double(w),
                               a=as.double(a),
                               e=as.double(e),
                               level=as.integer(level),
                               MAXITER=as.integer(MAXITER),
                               FCRIT=as.double(FCRIT),
                               fvalue=as.double(fvalue),
                               echo=as.integer( echo ), PACKAGE = "fmds" ) )

  r <- list( data = q,
             weights = w,
             transformed.data = result$q,
             approach = ifelse( level == 3, 1, ifelse( level == 4, 2, 0 ) ),
             degree = 0,
             ninner = 0,
             iknots = NULL,
             anchor = ifelse( level <= 1, FALSE, TRUE ),
             knotstype = 0,
             coordinates = NULL,
             coefficients = result$a,
             distances = result$e,
             last.iteration = result$MAXITER,
             last.difference = result$FCRIT,
             n.stress = result$fvalue,
             stress.1 = sqrt( result$fvalue ),
             call = match.call() )

  class(r) <- "fmds"
  r

}

explain <- function (x, ...) UseMethod( "explain" )
