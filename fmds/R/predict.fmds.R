#' Predict method for all fmds objects
#'
#' \code{predict} provides locations for additional objects.
#' Function is under construction.
#' 
#' @param object object of class \code{fmds}.
#' @param delta an n by n squares hollow matrix containing dissimilarities.
#' @param w an identical sized matrix containing non-negative weights (all ones when omitted).
#' @param level parameter indicating whether absolute, ratio, linear, or ordinal level to be used.
#' @param MAXITER maximum number of iterations (default = 1024).
#' @param FCRIT relative convergence criterion function value (default = 0.00000001).
#' @param error.check extensive validity check input parameters (default = FALSE).
#' @param echo print intermediate algorithm results (default = FALSE).
#' @param \dots additional arguments to be passed.
#' 
#' @return data original n by n matrix with dissimilarities.
#' @return weights original n by n matrix with weights.
#' @return transformed.data final n by n matrix with transformed dissimilarities.
#' @return approach apporach to ties: 1 = untie ties, 2 = keep ties tied.
#' @return degree spline degree.
#' @return ninner number of interior knots.
#' @return iknots interior knots sequence.
#' @return anchor whether an intervept was used or not.
#' @return knotstype type of procedure creating the interior knot sequence.
#' @return coordinates final n by p matrix with coordinates.
#' @return coefficients final h by p matrix with regression coefficients.
#' @return distances final n by n matrix with Euclidean distances between n rows of coordinates.
#' @return last.iteration final iteration number.
#' @return last.difference final function difference used for convergence testing.
#' @return n.stress final normalized stress value.
#'
#' @author Frank M.T.A. Busing
#' @export
#' @useDynLib fmds, .registration = TRUE
predict.fmds <- function(object,
						 delta,
						 w = NULL,
						 level = c( "absolute", "ratio", "linear", "ordinal" ),
						 MAXITER = 1024,
						 FCRIT = 0.00000001,
						 error.check = FALSE,
						 echo = FALSE, ...)
{

  x <- object

  n <- nrow( x$coordinates )
  p <- ncol( x$coordinates )
  if ( is.null( w ) ) w <- rep( 1, n )

    # check for input errors
  if ( error.check == TRUE ) {

    if ( !is.vector( delta ) ) stop( "'delta' must be a vector" )
    if ( length( delta ) != n ) stop( "'delta' must have appropriate length" )
    if ( anyNA( delta ) ) stop( "NA values not allowed in 'delta'" )

    if ( !is.vector( w ) ) stop( "'w' must be a vector" )
    if ( length( w ) != n ) stop( "'w' must have appropriate length" )
    if ( anyNA( w ) ) stop( "NA values not allowed in 'w'" )

    if ( MAXITER < 0 ) stop( "negative maximum number of iterations MAXITER not allowed")

    if ( FCRIT < 0.0 ) stop( "negative function convergence criterion not allowed" )
  }

  levels <- c( "absolute", "ratio", "linear", "ordinal" )
  if ( is.numeric( level ) ) level <- levels[level + 1]
  level <- match.arg( level, levels, several.ok = FALSE )
  if ( level == "absolute" ) level = 0
  else if ( level == "ratio" ) level = 1
  else if ( level == "linear" ) level = 2
  else if ( level == "ordinal" ) level = 3
  else level = 0

  z <- rep( 0, p )
  d <- rep( 0, n )
  if ( MAXITER < 0 ) MAXITER = 1024
  if ( FCRIT < 0.0 ) FCRIT = 0.00000001
  fvalue = 0.0

  result <- ( .C( "CRpredict", n=as.integer(n),
                               p=as.integer(p),
                               z=as.double(x$coordinates),
                               delta=as.double(delta),
                               w=as.double(w),
                               y=as.double(z),
                               d=as.double(d),
                               level=as.integer(level),
                               MAXITER=as.integer(MAXITER),
                               FCRIT=as.double(FCRIT),
                               fvalue=as.double(fvalue),
                               echo=as.integer( echo ), PACKAGE = "fmds" ) )

  r <- list( data = delta,
             weights = w,
             transformed.data = result$delta, 
             approach = ifelse( level == 3, 1, 0 ),
             degree = 0,
             ninner = 0,
             iknots = NULL,
             anchor = ifelse( level <= 1, FALSE, TRUE ),
             knotstype = 0,
             coordinates = result$y, 
             coefficients = NULL, 
             distances = result$d, 
             last.iteration = result$MAXITER, 
             last.difference = result$FCRIT, 
             n.stress = result$fvalue,
             call = match.call() )
  class(r) <- "fmds"
  r

}
