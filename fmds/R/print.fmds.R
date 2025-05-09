#' Print method for all fmds objects
#'
#' \code{print} produces output for fmds object.
#'
#' @param x object of class \code{fmds}.
#' @param \dots additional arguments to be passed.
#' 
#' @return none
#' @export
print.fmds<- function(x, ...)
{
  cat( "Call (fmds package)       : " )
  print( x$call )
  if ( grepl( "fastmds", as.character( x$call )[1], fixed = TRUE ) ) {
    cat( "Last iteration            :", x$last.iteration, "\n" )
    cat( "Last function difference  :", sprintf( x$last.difference, fmt = '%#.12f'), "\n" )
    cat( "Normalized Stress value   :", format( x$n.stress, nsmall = 12 ), "\n" )
    cat( "Stress-I value            :", format( sqrt( x$n.stress ), nsmall = 12 ), "\n" )
    cat( "\n" )
  }
  if ( grepl( "explain", as.character( x$call )[1], fixed = TRUE ) ) {
    cat( "Last iteration            :", x$last.iteration, "\n" )
    cat( "Variance Accounted For    :", format( x$n.stress, nsmall = 12), "\n" )
    cat( "Coefficients              : ", x$coefficients, "\n" )
    cat( "\n" )
  }
}
