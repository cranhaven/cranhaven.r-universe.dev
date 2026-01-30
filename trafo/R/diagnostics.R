#' Diagnostics for fitted models
#'
#' Returns information about the transformation and selected diagnostics to 
#' check model assumptions.
#' 
#' @param object an object that contains two models that should be compared. 
#' @param ... other parameters that can be passed to the function.
#' @return The return depends on the class of its argument. The 
#' documentation of particular methods gives detailed information about the 
#' return of that method.
#' @seealso  \code{\link{diagnostics.trafo_lm}}, 
#' \code{\link{diagnostics.trafo_compare}}
#' @keywords diagnostics
#' @export
diagnostics <- function(object,...) UseMethod("diagnostics")




