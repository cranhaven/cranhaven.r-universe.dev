#' Add an element to the back
#' 
#' Add an element to the back of a container by reference.
#' 
#' @param x A CppVector, CppDeque, or CppList object.
#' @param value A value to add to \code{x}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{back}, \link{emplace_back}, \link{insert}, \link{pop_back}, \link{push_front}.
#' 
#' @examples
#' v <- cpp_vector(4:6)
#' v
#' # 4 5 6
#' 
#' push_back(v, 14L)
#' v
#' # 4 5 6 14
#' 

#' @aliases push_back,CppVector-method push_back,CppDeque-method push_back,CppList-method

#' @export
methods::setGeneric("push_back", function(x, value) standardGeneric("push_back"))

#' @include classes.R

#' @export
methods::setMethod("push_back", methods::signature(x = "CppVector"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = vector_push_back_i(x@pointer, value),
    double = vector_push_back_d(x@pointer, value),
    string = vector_push_back_s(x@pointer, value),
    boolean = vector_push_back_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("push_back", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = deque_push_back_i(x@pointer, value),
    double = deque_push_back_d(x@pointer, value),
    string = deque_push_back_s(x@pointer, value),
    boolean = deque_push_back_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("push_back", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = deque_push_back_i(x@pointer, value),
    double = deque_push_back_d(x@pointer, value),
    string = deque_push_back_s(x@pointer, value),
    boolean = deque_push_back_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("push_back", methods::signature(x = "CppList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = list_push_back_i(x@pointer, value),
    double = list_push_back_d(x@pointer, value),
    string = list_push_back_s(x@pointer, value),
    boolean = list_push_back_b(x@pointer, value)
  ))
})
