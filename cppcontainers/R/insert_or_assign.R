#' Add or overwrite elements
#' 
#' Add elements to a container by reference. Overwrites existing container values tied to the same keys.
#' 
#' @param x A CppMap or CppUnorderedMap object.
#' @param values Values to add to \code{x}.
#' @param keys Keys to add to \code{x}.
#' 
#' @details Use \link{insert} to avoid overwriting values.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{insert}, \link{insert_after}, \code{emplace}, \code{try_emplace}.
#' 
#' @examples
#' m <- cpp_map(4:6, 9:11)
#' m
#' # [4,9] [5,10] [6,11]
#' 
#' insert_or_assign(m, 12:13, 6:7)
#' m
#' # [4,9] [5,10] [6,12] [7,13]
#' 

#' @aliases insert_or_assign,CppMap-method insert_or_assign,CppUnorderedMap-method

#' @export
methods::setGeneric("insert_or_assign", function(x, values, keys) standardGeneric("insert_or_assign"))

#' @include classes.R

#' @export
methods::setMethod("insert_or_assign", methods::signature(x = "CppMap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_insert_or_assign_i_i(x@pointer, keys, values),
      double = map_insert_or_assign_i_d(x@pointer, keys, values),
      string = map_insert_or_assign_i_s(x@pointer, keys, values),
      boolean = map_insert_or_assign_i_b(x@pointer, keys, values)),
    double = switch(x@value_type,
      integer = map_insert_or_assign_d_i(x@pointer, keys, values),
      double = map_insert_or_assign_d_d(x@pointer, keys, values),
      string = map_insert_or_assign_d_s(x@pointer, keys, values),
      boolean = map_insert_or_assign_d_b(x@pointer, keys, values)),
    string = switch(x@value_type,
      integer = map_insert_or_assign_s_i(x@pointer, keys, values),
      double = map_insert_or_assign_s_d(x@pointer, keys, values),
      string = map_insert_or_assign_s_s(x@pointer, keys, values),
      boolean = map_insert_or_assign_s_b(x@pointer, keys, values)),
    boolean = switch(x@value_type,
      integer = map_insert_or_assign_b_i(x@pointer, keys, values),
      double = map_insert_or_assign_b_d(x@pointer, keys, values),
      string = map_insert_or_assign_b_s(x@pointer, keys, values),
      boolean = map_insert_or_assign_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})

#' @export
methods::setMethod("insert_or_assign", methods::signature(x = "CppUnorderedMap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_insert_or_assign_i_i(x@pointer, keys, values),
      double = unordered_map_insert_or_assign_i_d(x@pointer, keys, values),
      string = unordered_map_insert_or_assign_i_s(x@pointer, keys, values),
      boolean = unordered_map_insert_or_assign_i_b(x@pointer, keys, values)),
    double = switch(x@value_type,
      integer = unordered_map_insert_or_assign_d_i(x@pointer, keys, values),
      double = unordered_map_insert_or_assign_d_d(x@pointer, keys, values),
      string = unordered_map_insert_or_assign_d_s(x@pointer, keys, values),
      boolean = unordered_map_insert_or_assign_d_b(x@pointer, keys, values)),
    string = switch(x@value_type,
      integer = unordered_map_insert_or_assign_s_i(x@pointer, keys, values),
      double = unordered_map_insert_or_assign_s_d(x@pointer, keys, values),
      string = unordered_map_insert_or_assign_s_s(x@pointer, keys, values),
      boolean = unordered_map_insert_or_assign_s_b(x@pointer, keys, values)),
    boolean = switch(x@value_type,
      integer = unordered_map_insert_or_assign_b_i(x@pointer, keys, values),
      double = unordered_map_insert_or_assign_b_d(x@pointer, keys, values),
      string = unordered_map_insert_or_assign_b_s(x@pointer, keys, values),
      boolean = unordered_map_insert_or_assign_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})
