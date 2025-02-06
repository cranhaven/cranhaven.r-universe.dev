#' Add an element to the back
#' 
#' Add an element to the back of a container by reference in place.
#' 
#' @param x A CppVector, CppDeque, CppList object.
#' @param value A value to add to \code{x}.
#' 
#' @details The emplace methods only add single elements. Use \link{insert} to add multiple elements in one call.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_after}, \link{emplace_front}, \link{insert}, \link{pop_back}, \link{push_back}.
#' 
#' @examples
#' v <- cpp_vector(4:6)
#' v
#' # 4 5 6
#' 
#' emplace_back(v, 12L)
#' v
#' # 4 5 6 12
#' 

#' @aliases emplace_back,CppVector-method emplace_back,CppDeque-method emplace_back,CppList-method

#' @export
methods::setGeneric("emplace_back", function(x, value) standardGeneric("emplace_back"))

#' @include classes.R

#' @export
methods::setMethod("emplace_back", methods::signature(x = "CppVector"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = vector_emplace_back_i(x@pointer, value),
    double = vector_emplace_back_d(x@pointer, value),
    string = vector_emplace_back_s(x@pointer, value),
    boolean = vector_emplace_back_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace_back", methods::signature(x = "CppDeque"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = deque_emplace_back_i(x@pointer, value),
    double = deque_emplace_back_d(x@pointer, value),
    string = deque_emplace_back_s(x@pointer, value),
    boolean = deque_emplace_back_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace_back", methods::signature(x = "CppList"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = list_emplace_back_i(x@pointer, value),
    double = list_emplace_back_d(x@pointer, value),
    string = list_emplace_back_s(x@pointer, value),
    boolean = list_emplace_back_b(x@pointer, value)
  ))
})
