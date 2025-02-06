#' Shrink container capacity to size
#' 
#' Shrink the capacity of a container to its size by reference.
#' 
#' @param x A CppVector or CppDeque object.
#' 
#' @details The capacity is the space, in terms of the number of elements, reserved for a container. The size is the number of elements in the container.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{capacity}, \link{reserve}, \link{size}.
#' 
#' @examples
#' v <- cpp_vector(4:6)
#' capacity(v)
#' # [1] 3
#' 
#' reserve(v, 10)
#' capacity(v)
#' # [1] 10
#' 
#' shrink_to_fit(v)
#' capacity(v)
#' # [1] 3
#' 

#' @aliases shrink_to_fit,CppVector-method shrink_to_fit,CppDeque-method

#' @export
methods::setGeneric("shrink_to_fit", function(x) standardGeneric("shrink_to_fit"))

#' @include classes.R

#' @export
methods::setMethod("shrink_to_fit", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_shrink_to_fit_i(x@pointer),
    double = vector_shrink_to_fit_d(x@pointer),
    string = vector_shrink_to_fit_s(x@pointer),
    boolean = vector_shrink_to_fit_b(x@pointer)
  ))
})

#' @export
methods::setMethod("shrink_to_fit", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_shrink_to_fit_i(x@pointer),
    double = deque_shrink_to_fit_d(x@pointer),
    string = deque_shrink_to_fit_s(x@pointer),
    boolean = deque_shrink_to_fit_b(x@pointer)
  ))
})
