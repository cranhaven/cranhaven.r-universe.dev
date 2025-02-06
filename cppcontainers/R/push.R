#' Add elements
#' 
#' Add elements to the top of a stack, to the back of a queue, or to a priority queue by reference.
#' 
#' @param x A CppStack, CppQueue, or CppPriorityQueue object.
#' @param values Values to add to \code{x}.
#' 
#' @details The method iterates through \code{values} starting at the front of the vector. I.e., the last element of \code{values} is added last.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{back}, \link{emplace}, \link{front}, \link{pop}, \link{push}, \link{top}.
#' 
#' @examples
#' s <- cpp_stack(1:4)
#' s
#' # Top element: 4
#' 
#' push(s, 8:9)
#' s
#' # Top element: 9
#' 

#' @aliases push,CppStack-method push,CppQueue-method push,CppPriorityQueue-method

#' @export
methods::setGeneric("push", function(x, values) standardGeneric("push"))

#' @include classes.R

#' @export
methods::setMethod("push", methods::signature(x = "CppStack"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = stack_push_i(x@pointer, values),
    double = stack_push_d(x@pointer, values),
    string = stack_push_s(x@pointer, values),
    boolean = stack_push_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("push", methods::signature(x = "CppQueue"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = queue_push_i(x@pointer, values),
    double = queue_push_d(x@pointer, values),
    string = queue_push_s(x@pointer, values),
    boolean = queue_push_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("push", methods::signature(x = "CppPriorityQueue"), function(x, values) {
  check_insert_values(values)
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_push_i_a(x@pointer, values),
      double = priority_queue_push_d_a(x@pointer, values),
      string = priority_queue_push_s_a(x@pointer, values),
      boolean = priority_queue_push_b_a(x@pointer, values)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_push_i_d(x@pointer, values),
      double = priority_queue_push_d_d(x@pointer, values),
      string = priority_queue_push_s_d(x@pointer, values),
      boolean = priority_queue_push_b_d(x@pointer, values)
    ))
  }
})
