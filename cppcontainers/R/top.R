#' Access top element
#' 
#' Access the top element in a container without removing it.
#' 
#' @param x A CppStack or CppPriorityQueue object.
#' 
#' @returns Returns the top element.
#' 
#' @seealso \link{emplace}, \link{pop}, \link{push}.
#' 
#' @examples
#' s <- cpp_stack(1:4)
#' s
#' # Top element: 4
#' 
#' top(s)
#' # [1] 4
#' 
#' s
#' # Top element: 4
#' 

#' @aliases top,CppStack-method top,CppPriorityQueue-method

#' @export
methods::setGeneric("top", function(x) standardGeneric("top"))

#' @include classes.R

#' @export
methods::setMethod("top", methods::signature(x = "CppStack"), function(x) {
  return(switch(x@type,
    integer = stack_top_i(x@pointer),
    double = stack_top_d(x@pointer),
    string = stack_top_s(x@pointer),
    boolean = stack_top_b(x@pointer)
  ))
})

#' @export
methods::setMethod("top", methods::signature(x = "CppPriorityQueue"), function(x) {
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_top_i_a(x@pointer),
      double = priority_queue_top_d_a(x@pointer),
      string = priority_queue_top_s_a(x@pointer),
      boolean = priority_queue_top_b_a(x@pointer)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_top_i_d(x@pointer),
      double = priority_queue_top_d_d(x@pointer),
      string = priority_queue_top_s_d(x@pointer),
      boolean = priority_queue_top_b_d(x@pointer)
    ))
  }
})
