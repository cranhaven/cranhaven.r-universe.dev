#' Add an element to the front
#' 
#' Add an element to the front of a container by reference.
#' 
#' @param x A CppDeque, CppForwardList, or CppList object.
#' @param value A value to add to \code{x}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace_front}, \link{front}, \link{insert}, \link{pop_front}, \link{push_back}.
#' 
#' @examples
#' d <- cpp_deque(4:6)
#' d
#' # 4 5 6
#' 
#' push_front(d, 14L)
#' d
#' # 14 4 5 6
#' 

#' @aliases push_front,CppDeque-method push_front,CppForwardList-method push_front,CppList-method

#' @export
methods::setGeneric("push_front", function(x, value) standardGeneric("push_front"))

#' @include classes.R

#' @export
methods::setMethod("push_front", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = deque_push_front_i(x@pointer, value),
    double = deque_push_front_d(x@pointer, value),
    string = deque_push_front_s(x@pointer, value),
    boolean = deque_push_front_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("push_front", methods::signature(x = "CppForwardList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = forward_list_push_front_i(x@pointer, value),
    double = forward_list_push_front_d(x@pointer, value),
    string = forward_list_push_front_s(x@pointer, value),
    boolean = forward_list_push_front_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("push_front", methods::signature(x = "CppList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = list_push_front_i(x@pointer, value),
    double = list_push_front_d(x@pointer, value),
    string = list_push_front_s(x@pointer, value),
    boolean = list_push_front_b(x@pointer, value)
  ))
})
