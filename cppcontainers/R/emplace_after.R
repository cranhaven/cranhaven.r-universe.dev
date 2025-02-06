#' Add an element
#' 
#' Add an element to a forward list by reference in place.
#' 
#' @param x A CppForwardList object.
#' @param value Value to add to \code{x}.
#' @param position Index after which to insert the element. Indices start at 1. The function does not perform bounds checks. Indices outside \code{x} crash 
#' the program.
#' 
#' @details The emplace methods only add single elements. Use \link{insert} to add multiple elements in one call.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_back}, \link{emplace_front}, \link{insert}.
#' 
#' @examples
#' l <- cpp_forward_list(4:6)
#' l
#' # 4 5 6
#' 
#' emplace_after(l, 10L, 2)
#' l
#' # 4 5 10 6
#' 

#' @aliases emplace_after,CppForwardList-method

#' @export
methods::setGeneric("emplace_after", function(x, value, position) standardGeneric("emplace_after"))

#' @include classes.R

#' @export
methods::setMethod("emplace_after", methods::signature(x = "CppForwardList"), function(x, value, position) {
  check_insert_value(value)
  position <- position - 1L
  return(switch(x@type,
    integer = forward_list_emplace_after_i(x@pointer, value, position),
    double = forward_list_emplace_after_d(x@pointer, value, position),
    string = forward_list_emplace_after_s(x@pointer, value, position),
    boolean = forward_list_emplace_after_b(x@pointer, value, position)
  ))
})
