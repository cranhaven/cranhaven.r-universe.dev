#' Move elements
#' 
#' Move elements from one forward list to another forward list by reference.
#' 
#' @param x A CppForwardList object to which to add elements.
#' @param y A CppForwardList object, of the same data type as \code{x}, from which to extract elements.
#' @param x_position Index after which to insert elements in \code{x}.
#' @param y_from Index after which to extract elements from \code{y}.
#' @param y_to Index of the last element to extract from \code{y}.
#' 
#' @details Indices start at 1, which is also the minimum value permitted. Thus, the current implementation in this package does not allow to move the 
#' first element of \code{y}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{merge}, \link{splice}.
#' 
#' @examples
#' x <- cpp_forward_list(4:9)
#' x
#' # 4 5 6 7 8 9
#' 
#' y <- cpp_forward_list(10:12)
#' y
#' # 10 11 12
#' 
#' splice_after(x, y, 3, 1, 3)
#' x
#' # 4 5 6 11 12 7 8 9
#' y
#' # 10
#' 

#' @aliases splice_after,CppForwardList-method

#' @export
methods::setGeneric("splice_after", function(x, y, x_position, y_from, y_to) standardGeneric("splice_after"))

#' @include classes.R

#' @export
methods::setMethod("splice_after", methods::signature(x = "CppForwardList"), function(x, y, x_position, y_from, y_to) {
  x_position <- x_position - 1L
  y_from <- y_from - 1L
  return(switch(x@type,
    integer = forward_list_splice_after_i(x@pointer, y@pointer, x_position, y_from, y_to),
    double = forward_list_splice_after_d(x@pointer, y@pointer, x_position, y_from, y_to),
    string = forward_list_splice_after_s(x@pointer, y@pointer, x_position, y_from, y_to),
    boolean = forward_list_splice_after_b(x@pointer, y@pointer, x_position, y_from, y_to)
  ))
})
