#' Remove top element
#' 
#' Remove top element in a stack or priority queue or the first element in a queue by reference.
#' 
#' @param x A CppStack, CppQueue, or CppPriorityQueue object.
#' 
#' @details In a stack, it is the last inserted element. In a queue, it is the first inserted element. In a descending (ascending) priority queue, it is 
#' the largest (smallest) value.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{back}, \link{emplace}, \link{front}, \link{push}, \link{top}.
#' 
#' @examples
#' s <- cpp_stack(4:6)
#' s
#' # Top element: 6
#' 
#' pop(s)
#' s
#' # Top element: 5
#' 
#' q <- cpp_queue(4:6)
#' q
#' # First element: 4
#' 
#' pop(q)
#' q
#' # First element: 5
#' 
#' p <- cpp_priority_queue(4:6)
#' p
#' # First element: 6
#' 
#' pop(p)
#' p
#' # First element: 5
#' 

#' @aliases pop,CppStack-method pop,CppQueue-method pop,CppPriorityQueue-method

#' @export
methods::setGeneric("pop", function(x) standardGeneric("pop"))

#' @include classes.R

#' @export
methods::setMethod("pop", methods::signature(x = "CppStack"), function(x) {
  return(switch(x@type,
    integer = stack_pop_i(x@pointer),
    double = stack_pop_d(x@pointer),
    string = stack_pop_s(x@pointer),
    boolean = stack_pop_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop", methods::signature(x = "CppQueue"), function(x) {
  return(switch(x@type,
    integer = queue_pop_i(x@pointer),
    double = queue_pop_d(x@pointer),
    string = queue_pop_s(x@pointer),
    boolean = queue_pop_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop", methods::signature(x = "CppPriorityQueue"), function(x) {
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_pop_i_a(x@pointer),
      double = priority_queue_pop_d_a(x@pointer),
      string = priority_queue_pop_s_a(x@pointer),
      boolean = priority_queue_pop_b_a(x@pointer)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_pop_i_d(x@pointer),
      double = priority_queue_pop_d_d(x@pointer),
      string = priority_queue_pop_s_d(x@pointer),
      boolean = priority_queue_pop_b_d(x@pointer)
    ))
  }
})
