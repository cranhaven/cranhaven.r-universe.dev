#' Replace all elements
#' 
#' Replace all elements in a container by reference.
#' 
#' @param x A CppVector, CppDeque, CppForwardList, or CppList object.
#' @param value A vector. It is called \code{value} instead of \code{values} for compliance with the generic \code{base::assign} method.
#' @param pos Ignored.
#' @param envir Ignored.
#' @param inherits Ignored.
#' @param immediate Ignored.
#' 
#' @details Replaces all elements in \code{x} with the elements in \code{value}.
#' 
#' The parameters \code{pos}, \code{envir}, \code{inherits}, and \code{immediate} are only included for compatibility with the generic \code{base::assign} 
#' method and have no effect.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_after}, \link{emplace_back}, \link{emplace_front}, \link{insert}, \link{insert_after}, \link{insert_or_assign}.
#' 
#' @examples
#' v <- cpp_vector(4:9)
#' v
#' # 4 5 6 7 8 9
#' 
#' assign(v, 12:14)
#' v
#' # 12 13 14
#' 

#' @aliases assign,CppVector-method assign,CppDeque-method assign,CppForwardList-method assign,CppList-method
#' @usage assign(x, value, pos, envir, inherits, immediate)
#' @export
methods::setGeneric("assign", methods::getGeneric("assign", package = "base"))

#' @include classes.R

#' @export
methods::setMethod("assign", methods::signature(x = "CppVector"), function(x, value) {
  check_insert_values(value)
  return(switch(x@type,
    integer = vector_assign_i(x@pointer, value),
    double = vector_assign_d(x@pointer, value),
    string = vector_assign_s(x@pointer, value),
    boolean = vector_assign_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("assign", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_values(value)
  return(switch(x@type,
    integer = deque_assign_i(x@pointer, value),
    double = deque_assign_d(x@pointer, value),
    string = deque_assign_s(x@pointer, value),
    boolean = deque_assign_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("assign", methods::signature(x = "CppForwardList"), function(x, value) {
  check_insert_values(value)
  return(switch(x@type,
    integer = forward_list_assign_i(x@pointer, value),
    double = forward_list_assign_d(x@pointer, value),
    string = forward_list_assign_s(x@pointer, value),
    boolean = forward_list_assign_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("assign", methods::signature(x = "CppList"), function(x, value) {
  check_insert_values(value)
  return(switch(x@type,
    integer = list_assign_i(x@pointer, value),
    double = list_assign_d(x@pointer, value),
    string = list_assign_s(x@pointer, value),
    boolean = list_assign_b(x@pointer, value)
  ))
})
