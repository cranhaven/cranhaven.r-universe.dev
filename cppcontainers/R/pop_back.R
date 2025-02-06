#' Remove an element from the back
#' 
#' Remove an element from the back of the container by reference.
#' 
#' @param x A CppVector, CppDeque, or CppList object.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{back}, \link{emplace_back}, \link{pop}, \link{pop_front}, \link{push_back}.
#' 
#' @examples
#' l <- cpp_list(4:6)
#' l
#' # 4 5 6
#' 
#' pop_back(l)
#' l
#' # 4 5
#' 

#' @aliases pop_back,CppVector-method pop_back,CppDeque-method pop_back,CppList-method

#' @export
methods::setGeneric("pop_back", function(x) standardGeneric("pop_back"))

#' @include classes.R

#' @export
methods::setMethod("pop_back", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_pop_back_i(x@pointer),
    double = vector_pop_back_d(x@pointer),
    string = vector_pop_back_s(x@pointer),
    boolean = vector_pop_back_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop_back", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_pop_back_i(x@pointer),
    double = deque_pop_back_d(x@pointer),
    string = deque_pop_back_s(x@pointer),
    boolean = deque_pop_back_b(x@pointer)
  ))
})

#' @export
methods::setMethod("pop_back", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_pop_back_i(x@pointer),
    double = list_pop_back_d(x@pointer),
    string = list_pop_back_s(x@pointer),
    boolean = list_pop_back_b(x@pointer)
  ))
})
