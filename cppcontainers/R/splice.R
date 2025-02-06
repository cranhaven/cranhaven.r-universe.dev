#' Move elements
#' 
#' Move elements from one list to another list by reference.
#' 
#' @param x A CppList object to which to add elements.
#' @param y A CppList object, of the same data type as \code{x}, from which to extract elements.
#' @param x_position Index at which to insert elements in \code{x}.
#' @param y_from Index of the first element to extract from \code{y}.
#' @param y_to Index of the last element to extract from \code{y}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{merge}, \link{splice_after}.
#' 
#' @examples
#' x <- cpp_list(4:9)
#' x
#' # 4 5 6 7 8 9
#' 
#' y <- cpp_list(10:12)
#' y
#' # 10 11 12
#' 
#' splice(x, y, 3, 2, 3)
#' x
#' # 4 5 11 12 6 7 8 9
#' y
#' # 10
#' 

#' @aliases splice,CppList-method

#' @export
methods::setGeneric("splice", function(x, y, x_position, y_from, y_to) standardGeneric("splice"))

#' @include classes.R

#' @export
methods::setMethod("splice", methods::signature(x = "CppList"), function(x, y, x_position, y_from, y_to) {
  x_position <- x_position - 1L
  y_from <- y_from - 1L
  return(switch(x@type,
    integer = list_splice_i(x@pointer, y@pointer, x_position, y_from, y_to),
    double = list_splice_d(x@pointer, y@pointer, x_position, y_from, y_to),
    string = list_splice_s(x@pointer, y@pointer, x_position, y_from, y_to),
    boolean = list_splice_b(x@pointer, y@pointer, x_position, y_from, y_to)
  ))
})
