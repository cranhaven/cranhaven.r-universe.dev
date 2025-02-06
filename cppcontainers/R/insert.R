#' Add elements
#' 
#' Add elements to a container by reference.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppVector, CppDeque, 
#' or CppList object.
#' @param values Values to add to \code{x}.
#' @param keys Keys to add to \code{x}. Only relevant for CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects.
#' @param position Index at which to insert elements into \code{x}. Only relevant for CppVector, CppDeque, and CppList objects. Indices start at 1.
#' 
#' @details Existing container values are not overwritten. I.e. inserting a key-value pair into a map that already contains that key preserves the old 
#' value and discards the new one. Use \link{insert_or_assign} to overwrite values.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{assign}, \link{emplace}, \link{insert_after}, \link{insert_or_assign}, \link{push}.
#' 
#' @examples
#' s <- cpp_multiset(4:6)
#' s
#' # 4 5 6
#' 
#' insert(s, 6:7)
#' s
#' # 4 5 6 6 7
#' 
#' m <- cpp_map(c("hello", "there", "world"), 9:11)
#' m
#' # ["hello",9] ["there",10] ["world",11]
#' 
#' insert(m, 12L, "there")
#' m
#' # ["hello",9] ["there",10] ["world",11]
#' 

#' @aliases insert,CppSet-method insert,CppUnorderedSet-method insert,CppMultiset-method insert,CppUnorderedMultiset-method insert,CppMap-method 
#' insert,CppUnorderedMap-method insert,CppMultimap-method insert,CppUnorderedMultimap-method insert,CppVector-method insert,CppDeque-method 
#' insert,CppList-method

#' @export
methods::setGeneric("insert", function(x, values, keys = NULL, position = NULL) standardGeneric("insert"))

#' @include classes.R

#' @export
methods::setMethod("insert", methods::signature(x = "CppSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = set_insert_i(x@pointer, values),
    double = set_insert_d(x@pointer, values),
    string = set_insert_s(x@pointer, values),
    boolean = set_insert_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppUnorderedSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_set_insert_i(x@pointer, values),
    double = unordered_set_insert_d(x@pointer, values),
    string = unordered_set_insert_s(x@pointer, values),
    boolean = unordered_set_insert_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = multiset_insert_i(x@pointer, values),
    double = multiset_insert_d(x@pointer, values),
    string = multiset_insert_s(x@pointer, values),
    boolean = multiset_insert_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppUnorderedMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_multiset_insert_i(x@pointer, values),
    double = unordered_multiset_insert_d(x@pointer, values),
    string = unordered_multiset_insert_s(x@pointer, values),
    boolean = unordered_multiset_insert_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppMap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = map_insert_i_i(x@pointer, keys, values),
        double = map_insert_i_d(x@pointer, keys, values),
        string = map_insert_i_s(x@pointer, keys, values),
        boolean = map_insert_i_b(x@pointer, keys, values)),
      double = switch(x@value_type,
        integer = map_insert_d_i(x@pointer, keys, values),
        double = map_insert_d_d(x@pointer, keys, values),
        string = map_insert_d_s(x@pointer, keys, values),
        boolean = map_insert_d_b(x@pointer, keys, values)),
      string = switch(x@value_type,
        integer = map_insert_s_i(x@pointer, keys, values),
        double = map_insert_s_d(x@pointer, keys, values),
        string = map_insert_s_s(x@pointer, keys, values),
        boolean = map_insert_s_b(x@pointer, keys, values)),
      boolean = switch(x@value_type,
        integer = map_insert_b_i(x@pointer, keys, values),
        double = map_insert_b_d(x@pointer, keys, values),
        string = map_insert_b_s(x@pointer, keys, values),
        boolean = map_insert_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppUnorderedMap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_map_insert_i_i(x@pointer, keys, values),
        double = unordered_map_insert_i_d(x@pointer, keys, values),
        string = unordered_map_insert_i_s(x@pointer, keys, values),
        boolean = unordered_map_insert_i_b(x@pointer, keys, values)),
      double = switch(x@value_type,
        integer = unordered_map_insert_d_i(x@pointer, keys, values),
        double = unordered_map_insert_d_d(x@pointer, keys, values),
        string = unordered_map_insert_d_s(x@pointer, keys, values),
        boolean = unordered_map_insert_d_b(x@pointer, keys, values)),
      string = switch(x@value_type,
        integer = unordered_map_insert_s_i(x@pointer, keys, values),
        double = unordered_map_insert_s_d(x@pointer, keys, values),
        string = unordered_map_insert_s_s(x@pointer, keys, values),
        boolean = unordered_map_insert_s_b(x@pointer, keys, values)),
      boolean = switch(x@value_type,
        integer = unordered_map_insert_b_i(x@pointer, keys, values),
        double = unordered_map_insert_b_d(x@pointer, keys, values),
        string = unordered_map_insert_b_s(x@pointer, keys, values),
        boolean = unordered_map_insert_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppMultimap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = multimap_insert_i_i(x@pointer, keys, values),
        double = multimap_insert_i_d(x@pointer, keys, values),
        string = multimap_insert_i_s(x@pointer, keys, values),
        boolean = multimap_insert_i_b(x@pointer, keys, values)),
      double = switch(x@value_type,
        integer = multimap_insert_d_i(x@pointer, keys, values),
        double = multimap_insert_d_d(x@pointer, keys, values),
        string = multimap_insert_d_s(x@pointer, keys, values),
        boolean = multimap_insert_d_b(x@pointer, keys, values)),
      string = switch(x@value_type,
        integer = multimap_insert_s_i(x@pointer, keys, values),
        double = multimap_insert_s_d(x@pointer, keys, values),
        string = multimap_insert_s_s(x@pointer, keys, values),
        boolean = multimap_insert_s_b(x@pointer, keys, values)),
      boolean = switch(x@value_type,
        integer = multimap_insert_b_i(x@pointer, keys, values),
        double = multimap_insert_b_d(x@pointer, keys, values),
        string = multimap_insert_b_s(x@pointer, keys, values),
        boolean = multimap_insert_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppUnorderedMultimap"), function(x, values, keys) {
  check_insert_values(keys)
  check_insert_values(values)
  if(length(keys) == length(values)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_multimap_insert_i_i(x@pointer, keys, values),
        double = unordered_multimap_insert_i_d(x@pointer, keys, values),
        string = unordered_multimap_insert_i_s(x@pointer, keys, values),
        boolean = unordered_multimap_insert_i_b(x@pointer, keys, values)),
      double = switch(x@value_type,
        integer = unordered_multimap_insert_d_i(x@pointer, keys, values),
        double = unordered_multimap_insert_d_d(x@pointer, keys, values),
        string = unordered_multimap_insert_d_s(x@pointer, keys, values),
        boolean = unordered_multimap_insert_d_b(x@pointer, keys, values)),
      string = switch(x@value_type,
        integer = unordered_multimap_insert_s_i(x@pointer, keys, values),
        double = unordered_multimap_insert_s_d(x@pointer, keys, values),
        string = unordered_multimap_insert_s_s(x@pointer, keys, values),
        boolean = unordered_multimap_insert_s_b(x@pointer, keys, values)),
      boolean = switch(x@value_type,
        integer = unordered_multimap_insert_b_i(x@pointer, keys, values),
        double = unordered_multimap_insert_b_d(x@pointer, keys, values),
        string = unordered_multimap_insert_b_s(x@pointer, keys, values),
        boolean = unordered_multimap_insert_b_b(x@pointer, keys, values))
    ))
  } else {
    stop("keys and values vectors must be of equal length")
  }
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppVector"), function(x, values, position) {
  check_insert_values(values)
  position <- position - 1L
  return(switch(x@type,
    integer = vector_insert_i(x@pointer, values, position),
    double = vector_insert_d(x@pointer, values, position),
    string = vector_insert_s(x@pointer, values, position),
    boolean = vector_insert_b(x@pointer, values, position)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppDeque"), function(x, values, position) {
  check_insert_values(values)
  position <- position - 1L
  return(switch(x@type,
    integer = deque_insert_i(x@pointer, values, position),
    double = deque_insert_d(x@pointer, values, position),
    string = deque_insert_s(x@pointer, values, position),
    boolean = deque_insert_b(x@pointer, values, position)
  ))
})

#' @export
methods::setMethod("insert", methods::signature(x = "CppList"), function(x, values, position) {
  check_insert_values(values)
  position <- position - 1L
  return(switch(x@type,
    integer = list_insert_i(x@pointer, values, position),
    double = list_insert_d(x@pointer, values, position),
    string = list_insert_s(x@pointer, values, position),
    boolean = list_insert_b(x@pointer, values, position)
  ))
})
