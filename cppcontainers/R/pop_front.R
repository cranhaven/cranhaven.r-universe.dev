#' Remove an element from the front
#' 
#' Remove an element from the front of the container by reference.
#' 
#' @param x A CppDeque, CppForwardList, or CppList object.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace_front}, \link{front}, \link{pop}, \link{pop_back}, \link{push_front}.
#' 
#' @examples
#' d <- cpp_deque(4:6)
#' d
#' # 4 5 6
#' 
#' pop_front(d)
#' d
#' # 5 6
#' 

#' @aliases pop_front,CppDeque-method pop_front,CppForwardList-method pop_front,CppList-method

#' @export
methods::setGeneric("pop_front", function(x) standardGeneric("pop_front"))

#' @include classes.R

#' @export
methods::setMethod("pop_front", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_pop_front_i(x@pointer),
    double = deque_pop_front_d(x@pointer),
    string = deque_pop_front_s(x@pointer),
    boolean = deque_pop_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop_front", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_pop_front_i(x@pointer),
    double = forward_list_pop_front_d(x@pointer),
    string = forward_list_pop_front_s(x@pointer),
    boolean = forward_list_pop_front_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop_front", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_pop_front_i(x@pointer),
    double = list_pop_front_d(x@pointer),
    string = list_pop_front_s(x@pointer),
    boolean = list_pop_front_b(x@pointer)
  ))
})
