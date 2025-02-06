#' Remove elements
#' 
#' Remove elements from a container by reference.
#' 
#' @param x A CppForwardList or CppList object.
#' @param value A value to delete from \code{x}.
#' 
#' @details The method ends with a dot to avoid compatibility issues with the generic \code{base::remove}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{clear}, \link{empty}, \link{erase}.
#' 
#' @examples
#' l <- cpp_forward_list(4:6)
#' l
#' # 4 5 6
#' 
#' remove.(l, 5L)
#' l
#' # 4 6
#' 

#' @aliases remove.,CppForwardList-method remove.,CppList-method

#' @export
methods::setGeneric("remove.", function(x, value) standardGeneric("remove."))

#' @include classes.R

#' @export
methods::setMethod("remove.", methods::signature(x = "CppForwardList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = forward_list_remove_i(x@pointer, value),
    double = forward_list_remove_d(x@pointer, value),
    string = forward_list_remove_s(x@pointer, value),
    boolean = forward_list_remove_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("remove.", methods::signature(x = "CppList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = list_remove_i(x@pointer, value),
    double = list_remove_d(x@pointer, value),
    string = list_remove_s(x@pointer, value),
    boolean = list_remove_b(x@pointer, value)
  ))
})
