#' Print method for all fmdu objects
#'
#' Print the results of a fmdu object
#'
#' @param x object of class \code{fmdu}.
#' @param \dots additional arguments to be passed.
#'
#' @return No return value, called for side effects (print)
#'
#' @export
print.fmdu<- function(x, ...)
{
  cat( "Call (fmdu package)      : " )
  print( x$call )
  cat( "Last iteration           :", x$last.iteration, "\n" )
  cat( "Last function difference :", sprintf( x$last.difference, fmt = '%#.12f'), "\n" )
  cat( "Normalized Stress value  :", format( x$n.stress, nsmall = 12 ), "\n" )
  cat( "Stress-I value           :", format( x$stress.1, nsmall = 12 ), "\n" )
  cat( "\n" )
}
