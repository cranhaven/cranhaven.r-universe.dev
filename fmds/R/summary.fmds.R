#' Summary method for all fmds objects
#'
#' \code{summary} produces an output summary for fmds objects.
#'
#' @param object object of class \code{fmds}.
#' @param \dots additional arguments to be passed.
#' 
#' @return none
#' @export
summary.fmds <- function(object, ...)
{
  x<-object
  cat( "Call (fmds package)      : " )
  print( x$call )
  cat( "Last iteration           :", x$last.iteration, "\n" )
  cat( "Last function difference :", sprintf( x$last.difference, fmt = '%#.12f'), "\n" )
  cat( "Normalized Stress value  :", format( x$n.stress, nsmall = 12 ), "\n" )
  cat( "Stress-I value           :", format( sqrt( x$n.stress ), nsmall = 12 ), "\n" )
  cat( "\n" )
}
