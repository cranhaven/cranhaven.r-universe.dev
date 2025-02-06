#' Add an element to the front
#' 
#' Add an element to the front of a container by reference in place.
#' 
#' @param x A CppDeque, CppForwardList, or CppList object.
#' @param value A value to add to \code{x}.
#' 
#' @details The emplace methods only add single elements. Use \link{insert} to add multiple elements in one call.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_after}, \link{emplace_back}, \link{insert}, \link{pop_front}, \link{push_front}.
#' 
#' @examples
#' l <- cpp_forward_list(4:6)
#' l
#' # 4 5 6
#' 
#' emplace_front(l, 12L)
#' l
#' # 12 4 5 6
#' 

#' @aliases emplace_front,CppDeque-method emplace_front,CppForwardList-method emplace_front,CppList-method

#' @export
methods::setGeneric("emplace_front", function(x, value) standardGeneric("emplace_front"))

#' @include classes.R

#' @export
methods::setMethod("emplace_front", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = deque_emplace_front_i(x@pointer, value),
    double = deque_emplace_front_d(x@pointer, value),
    string = deque_emplace_front_s(x@pointer, value),
    boolean = deque_emplace_front_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace_front", methods::signature(x = "CppForwardList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = forward_list_emplace_front_i(x@pointer, value),
    double = forward_list_emplace_front_d(x@pointer, value),
    string = forward_list_emplace_front_s(x@pointer, value),
    boolean = forward_list_emplace_front_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace_front", methods::signature(x = "CppList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = list_emplace_front_i(x@pointer, value),
    double = list_emplace_front_d(x@pointer, value),
    string = list_emplace_front_s(x@pointer, value),
    boolean = list_emplace_front_b(x@pointer, value)
  ))
})
