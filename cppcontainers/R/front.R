#' Access first element
#' 
#' Access first element in a container without removing it.
#' 
#' @param x A CppQueue, CppVector, CppDeque, CppForwardList, or CppList object.
#' 
#' @returns Returns the front element.
#' 
#' @seealso \link{back}, \link{emplace_front}, \link{pop}, \link{pop_front}, \link{push_front}.
#' 
#' @examples
#' q <- cpp_queue(4:6)
#' q
#' # First element: 4
#' 
#' front(q)
#' # [1] 4
#' 
#' q
#' # First element: 4
#' 
#' v <- cpp_vector(c(TRUE, FALSE, FALSE))
#' v
#' # TRUE FALSE FALSE
#' 
#' front(v)
#' # [1] TRUE
#' 

#' @aliases front,CppQueue-method front,CppVector-method front,CppDeque-method front,CppForwardList-method front,CppList-method

#' @export
methods::setGeneric("front", function(x) standardGeneric("front"))

#' @include classes.R

#' @export
methods::setMethod("front", methods::signature(x = "CppQueue"), function(x) {
  return(switch(x@type,
    integer = queue_front_i(x@pointer),
    double = queue_front_d(x@pointer),
    string = queue_front_s(x@pointer),
    boolean = queue_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("front", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_front_i(x@pointer),
    double = vector_front_d(x@pointer),
    string = vector_front_s(x@pointer),
    boolean = vector_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("front", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_front_i(x@pointer),
    double = deque_front_d(x@pointer),
    string = deque_front_s(x@pointer),
    boolean = deque_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("front", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_front_i(x@pointer),
    double = forward_list_front_d(x@pointer),
    string = forward_list_front_s(x@pointer),
    boolean = forward_list_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("front", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_front_i(x@pointer),
    double = list_front_d(x@pointer),
    string = list_front_s(x@pointer),
    boolean = list_front_b(x@pointer)
  ))
})
