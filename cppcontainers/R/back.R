#' Access last element
#' 
#' Access the last element in a container without removing it.
#' 
#' @param x A CppQueue, CppVector, CppDeque, or CppList object.
#' 
#' @details In a CppQueue, the last element is the last inserted one.
#' 
#' @returns Returns the last element.
#' 
#' @seealso \link{front}, \link{top}, \link{push_back}, \link{emplace_back}, \link{pop_back}.
#' 
#' @examples
#' q <- cpp_queue(1:4)
#' q
#' # First element: 1
#' 
#' back(q)
#' # [1] 4
#' 
#' l <- cpp_list(1:4)
#' l
#' # 1 2 3 4
#' 
#' back(l)
#' # [1] 4
#' 

#' @aliases back,CppQueue-method back,CppVector-method back,CppDeque-method back,CppList-method

#' @export
methods::setGeneric("back", function(x) standardGeneric("back"))

#' @include classes.R

#' @export
methods::setMethod("back", methods::signature(x = "CppQueue"), function(x) {
  return(switch(x@type,
    integer = queue_back_i(x@pointer),
    double = queue_back_d(x@pointer),
    string = queue_back_s(x@pointer),
    boolean = queue_back_b(x@pointer)
  ))
})

#' @export
methods::setMethod("back", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_back_i(x@pointer),
    double = vector_back_d(x@pointer),
    string = vector_back_s(x@pointer),
    boolean = vector_back_b(x@pointer)
  ))
})

#' @export
methods::setMethod("back", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_back_i(x@pointer),
    double = deque_back_d(x@pointer),
    string = deque_back_s(x@pointer),
    boolean = deque_back_b(x@pointer)
  ))
})

#' @export
methods::setMethod("back", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_back_i(x@pointer),
    double = list_back_d(x@pointer),
    string = list_back_s(x@pointer),
    boolean = list_back_b(x@pointer)
  ))
})
