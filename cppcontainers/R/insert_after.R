#' Add elements
#' 
#' Add elements to a forward list by reference.
#' 
#' @param x A CppForwardList object.
#' @param values Values to add to \code{x}.
#' @param position Index behind which to insert elements.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_after}, \link{insert}, \link{insert_or_assign}.
#' 
#' @examples
#' v <- cpp_forward_list(4:6)
#' v
#' # 4 5 6
#' 
#' insert_after(v, 10:11, 2)
#' v
#' # 4 5 10 11 6
#' 

#' @aliases insert_after,CppForwardList-method

#' @export
methods::setGeneric("insert_after", function(x, values, position = NULL) standardGeneric("insert_after"))

#' @include classes.R

#' @export
methods::setMethod("insert_after", methods::signature(x = "CppForwardList"), function(x, values, position) {
  check_insert_values(values)
  position <- position - 1L
  return(switch(x@type,
    integer = forward_list_insert_after_i(x@pointer, values, position),
    double = forward_list_insert_after_d(x@pointer, values, position),
    string = forward_list_insert_after_s(x@pointer, values, position),
    boolean = forward_list_insert_after_b(x@pointer, values, position)
  ))
})
