#' Summary method for all fmdu objects
#'
#'
#' @param object object of class \code{fmdu}.
#' @param \dots additional arguments to be passed.
#'
#' @return No return value, called for side effects (summary)
#'
#' @export
summary.fmdu <- function(object, ...)
{
  x<-object
  cat( "Call (fmdu package)      : " )
  print( x$call )
  cat( "Last iteration           :", x$last.iteration, "\n" )
  cat( "Last function difference :", sprintf( x$last.difference, fmt = '%#.12f'), "\n" )
  cat( "Normalized Stress value  :", format( x$n.stress, nsmall = 12 ), "\n" )
  cat( "Stress-I value           :", format( x$stress.1, nsmall = 12 ), "\n" )
  cat( "\n" )
}
