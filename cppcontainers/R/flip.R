#' Toggle boolean values
#' 
#' Toggle boolean values in a vector.
#' 
#' @param x A CppVector object of type boolean.
#' 
#' @details Sets TRUE to FALSE and FALSE to TRUE.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{cpp_vector}.
#' 
#' @examples
#' v <- cpp_vector(c(TRUE, TRUE, FALSE))
#' v
#' # TRUE TRUE FALSE
#' 
#' flip(v)
#' v
#' # FALSE FALSE TRUE
#' 

#' @aliases flip,CppVector-method

#' @export
methods::setGeneric("flip", function(x) standardGeneric("flip"))

#' @include classes.R

#' @export
methods::setMethod("flip", methods::signature(x = "CppVector"), function(x) {
  return(vector_flip_b(x@pointer))
})
