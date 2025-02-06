#' Get container capacity
#' 
#' Get the capacity of a CppVector.
#' 
#' @param x A CppVector object.
#' 
#' @details The capacity is the space reserved for the vector, which can exceed its \link{size}. Additional capacity ensures that the vector can be 
#' extended efficiently, without having to reallocate its current elements to a new memory location.
#' 
#' @returns Returns a numeric.
#' 
#' @seealso \link{reserve}, \link{shrink_to_fit}, \link{size}.
#' 
#' @examples
#' v <- cpp_vector(4:9)
#' v
#' # 4 5 6 7 8 9
#' 
#' capacity(v)
#' # [1] 6
#' 
#' reserve(v, 20)
#' size(v)
#' #[1] 6
#' capacity(v)
#' # [1] 20
#' 
#' v
#' # 4 5 6 7 8 9
#' 

#' @aliases capacity,CppVector-method

#' @export
methods::setGeneric("capacity", function(x) standardGeneric("capacity"))

#' @include classes.R

#' @export
methods::setMethod("capacity", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_capacity_i(x@pointer),
    double = vector_capacity_d(x@pointer),
    string = vector_capacity_s(x@pointer),
    boolean = vector_capacity_b(x@pointer)
  ))
})
