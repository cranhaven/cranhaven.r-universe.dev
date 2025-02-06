#' Erase elements
#' 
#' Delete elements from a forward list by reference.
#' 
#' @param x A CppForwardList object.
#' @param from Index after which to delete.
#' @param to Index until including which to delete. Indices start at 1. The function does not perform bounds checks. Indices outside \code{x} crash the 
#' program.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{clear}, \link{empty}, \link{erase}, \link{remove.}.
#' 
#' @examples
#' l <- cpp_forward_list(4:9)
#' l
#' # 4 5 6 7 8 9
#' 
#' erase_after(l, 2L, 4L)
#' l
#' # 4 5 8 9
#' 

#' @aliases erase_after,CppForwardList-method

#' @export
methods::setGeneric("erase_after", function(x, from, to) standardGeneric("erase_after"))

#' @include classes.R

#' @export
methods::setMethod("erase_after", methods::signature(x = "CppForwardList"), function(x, from, to) {
  from <- from - 1L
  return(switch(x@type,
    integer = forward_list_erase_after_i(x@pointer, from, to),
    double = forward_list_erase_after_d(x@pointer, from, to),
    string = forward_list_erase_after_s(x@pointer, from, to),
    boolean = forward_list_erase_after_b(x@pointer, from, to)
  ))
})
