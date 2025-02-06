#' Access elements with bounds checking
#' 
#' Read a value at a certain position with bounds checking.
#' 
#' @param x A CppMap, CppUnorderedMap, CppVector, or CppDeque object.
#' @param position A key (CppMap, CppUnorderedMap) or index (CppVector, CppDeque).
#' 
#' @details In the two associative container types (CppMap, CppUnorderedMap), \code{[]} accesses a value by its key. If the key does not exist, the 
#' function throws an error.
#' 
#' In the two sequence container types (CppVector, CppDeque), \code{[]} accesses a value by its index. If the index is outside the container, this throws 
#' an error.
#' 
#' \link{at} and \code{[]} both access elements. Unlike \code{[]}, \link{at} checks the bounds of the container and throws an error, if the element does 
#' not exist.
#' 
#' @returns Returns the value at the position.
#' 
#' @seealso \link{[}, \link{back}, \link{contains}, \link{front}, \link{top}.
#' 
#' @examples
#' m <- cpp_map(4:6, seq.int(0, 1, by = 0.5))
#' m
#' # [4,0] [5,0.5] [6,1]
#' 
#' at(m, 4L)
#' # [1] 0
#' 
#' d <- cpp_deque(c("hello", "world"))
#' d
#' # "hello" "world"
#' 
#' at(d, 2)
#' # [1] "world"
#' 

#' @aliases at,CppMap-method at,CppUnorderedMap-method at,CppVector-method at,CppDeque-method

#' @export
methods::setGeneric("at", function(x, position) standardGeneric("at"))

#' @include classes.R

#' @export
methods::setMethod("at", methods::signature(x = "CppMap"), function(x, position) {
  check_insert_value(position)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_at_i_i(x@pointer, position),
      double = map_at_i_d(x@pointer, position),
      string = map_at_i_s(x@pointer, position),
      boolean = map_at_i_b(x@pointer, position)),
    double = switch(x@value_type,
      integer = map_at_d_i(x@pointer, position),
      double = map_at_d_d(x@pointer, position),
      string = map_at_d_s(x@pointer, position),
      boolean = map_at_d_b(x@pointer, position)),
    string = switch(x@value_type,
      integer = map_at_s_i(x@pointer, position),
      double = map_at_s_d(x@pointer, position),
      string = map_at_s_s(x@pointer, position),
      boolean = map_at_s_b(x@pointer, position)),
    boolean = switch(x@value_type,
      integer = map_at_b_i(x@pointer, position),
      double = map_at_b_d(x@pointer, position),
      string = map_at_b_s(x@pointer, position),
      boolean = map_at_b_b(x@pointer, position))
  ))
})

#' @export
methods::setMethod("at", methods::signature(x = "CppUnorderedMap"), function(x, position) {
  check_insert_value(position)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_at_i_i(x@pointer, position),
      double = unordered_map_at_i_d(x@pointer, position),
      string = unordered_map_at_i_s(x@pointer, position),
      boolean = unordered_map_at_i_b(x@pointer, position)),
    double = switch(x@value_type,
      integer = unordered_map_at_d_i(x@pointer, position),
      double = unordered_map_at_d_d(x@pointer, position),
      string = unordered_map_at_d_s(x@pointer, position),
      boolean = unordered_map_at_d_b(x@pointer, position)),
    string = switch(x@value_type,
      integer = unordered_map_at_s_i(x@pointer, position),
      double = unordered_map_at_s_d(x@pointer, position),
      string = unordered_map_at_s_s(x@pointer, position),
      boolean = unordered_map_at_s_b(x@pointer, position)),
    boolean = switch(x@value_type,
      integer = unordered_map_at_b_i(x@pointer, position),
      double = unordered_map_at_b_d(x@pointer, position),
      string = unordered_map_at_b_s(x@pointer, position),
      boolean = unordered_map_at_b_b(x@pointer, position))
  ))
})

#' @export
methods::setMethod("at", methods::signature(x = "CppVector"), function(x, position) {
  position <- position - 1L
  return(switch(x@type,
    integer = vector_at_i(x@pointer, position),
    double = vector_at_d(x@pointer, position),
    string = vector_at_s(x@pointer, position),
    boolean = vector_at_b(x@pointer, position)
  ))
})

#' @export
methods::setMethod("at", methods::signature(x = "CppDeque"), function(x, position) {
  position <- position - 1L
  return(switch(x@type,
    integer = deque_at_i(x@pointer, position),
    double = deque_at_d(x@pointer, position),
    string = deque_at_s(x@pointer, position),
    boolean = deque_at_b(x@pointer, position)
  ))
})
