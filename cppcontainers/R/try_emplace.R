#' Add an element
#' 
#' Add an element to a container by reference in place, if it does not exist yet.
#' 
#' @param x A CppMap or CppUnorderedMap object.
#' @param value A value to add to \code{x}.
#' @param key A key to add to \code{x}.
#' 
#' @details Existing container values are not overwritten. I.e., inserting a key-value pair into a map that already contains that key preserves the old 
#' value and discards the new one. Use \link{insert_or_assign} to overwrite values.
#' 
#' \link{emplace} and \code{try_emplace} produce the same results in the context of this package. \code{try_emplace} can be minimally more computationally 
#' efficient than \link{emplace}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{emplace}, \link{emplace_after}, \link{emplace_back}, \link{emplace_front}, \link{insert}, \link{insert_or_assign}.
#' 
#' @examples
#' m <- cpp_map(4:6, 9:11)
#' m
#' # [4,9] [5,10] [6,11]
#' 
#' try_emplace(m, 13L, 8L)
#' m
#' # [4,9] [5,10] [6,11] [8,13]
#' 
#' try_emplace(m, 12L, 4L)
#' m
#' # [4,9] [5,10] [6,11] [8,13]
#' 

#' @aliases try_emplace,CppMap-method try_emplace,CppUnorderedMap-method

#' @export
methods::setGeneric("try_emplace", function(x, value, key) standardGeneric("try_emplace"))

#' @include classes.R

#' @export
methods::setMethod("try_emplace", methods::signature(x = "CppMap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  if(length(key) == 1L && length(value) == 1L) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = map_try_emplace_i_i(x@pointer, key, value),
        double = map_try_emplace_i_d(x@pointer, key, value),
        string = map_try_emplace_i_s(x@pointer, key, value),
        boolean = map_try_emplace_i_b(x@pointer, key, value)),
      double = switch(x@value_type,
        integer = map_try_emplace_d_i(x@pointer, key, value),
        double = map_try_emplace_d_d(x@pointer, key, value),
        string = map_try_emplace_d_s(x@pointer, key, value),
        boolean = map_try_emplace_d_b(x@pointer, key, value)),
      string = switch(x@value_type,
        integer = map_try_emplace_s_i(x@pointer, key, value),
        double = map_try_emplace_s_d(x@pointer, key, value),
        string = map_try_emplace_s_s(x@pointer, key, value),
        boolean = map_try_emplace_s_b(x@pointer, key, value)),
      boolean = switch(x@value_type,
        integer = map_try_emplace_b_i(x@pointer, key, value),
        double = map_try_emplace_b_d(x@pointer, key, value),
        string = map_try_emplace_b_s(x@pointer, key, value),
        boolean = map_try_emplace_b_b(x@pointer, key, value))
    ))
  } else {
    stop("try_emplace currently only accepts a single value. Use insert to add multiple values at once.")
  }
})

#' @export
methods::setMethod("try_emplace", methods::signature(x = "CppUnorderedMap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  if(length(key) == 1L && length(value) == 1L) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_map_try_emplace_i_i(x@pointer, key, value),
        double = unordered_map_try_emplace_i_d(x@pointer, key, value),
        string = unordered_map_try_emplace_i_s(x@pointer, key, value),
        boolean = unordered_map_try_emplace_i_b(x@pointer, key, value)),
      double = switch(x@value_type,
        integer = unordered_map_try_emplace_d_i(x@pointer, key, value),
        double = unordered_map_try_emplace_d_d(x@pointer, key, value),
        string = unordered_map_try_emplace_d_s(x@pointer, key, value),
        boolean = unordered_map_try_emplace_d_b(x@pointer, key, value)),
      string = switch(x@value_type,
        integer = unordered_map_try_emplace_s_i(x@pointer, key, value),
        double = unordered_map_try_emplace_s_d(x@pointer, key, value),
        string = unordered_map_try_emplace_s_s(x@pointer, key, value),
        boolean = unordered_map_try_emplace_s_b(x@pointer, key, value)),
      boolean = switch(x@value_type,
        integer = unordered_map_try_emplace_b_i(x@pointer, key, value),
        double = unordered_map_try_emplace_b_d(x@pointer, key, value),
        string = unordered_map_try_emplace_b_s(x@pointer, key, value),
        boolean = unordered_map_try_emplace_b_b(x@pointer, key, value))
    ))
  } else {
    stop("try_emplace currently only accepts a single value. Use insert to add multiple values at once.")
  }
})
