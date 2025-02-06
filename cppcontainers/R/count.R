#' Count element frequency
#' 
#' Count how often elements occur in a container.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, or CppUnorderedMultimap object.
#' @param values A vector of elements to check. Refers to keys in the case of CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects.
#' 
#' @returns Returns a vector of the same length as \code{values}, denoting for each value how often it occurs.
#' 
#' @seealso \link{[}, \link{==}, \link{at}, \link{contains}, \link{size}, \link{empty}.
#' 
#' @examples
#' s <- cpp_set(4:9)
#' s
#' # 4 5 6 7 8 9
#' 
#' count(s, 9:11)
#' # [1] 1 0 0
#' 
#' m <- cpp_map(c("hello", "there"), c(1.2, 1.3))
#' m
#' # ["hello",1.2] ["there",1.3]
#' 
#' count(m, c("hello", "world"))
#' # [1] 1 0
#' 

#' @aliases count,CppSet-method count,CppUnorderedSet-method count,CppMultiset-method count,CppUnorderedMultiset-method count,CppMap-method 
#' count,CppUnorderedMap-method count,CppMultimap-method count,CppUnorderedMultimap-method

#' @export
methods::setGeneric("count", function(x, values) standardGeneric("count"))

#' @include classes.R

#' @export
methods::setMethod("count", methods::signature(x = "CppSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = set_count_i(x@pointer, values),
    double = set_count_d(x@pointer, values),
    string = set_count_s(x@pointer, values),
    boolean = set_count_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppUnorderedSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_set_count_i(x@pointer, values),
    double = unordered_set_count_d(x@pointer, values),
    string = unordered_set_count_s(x@pointer, values),
    boolean = unordered_set_count_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = multiset_count_i(x@pointer, values),
    double = multiset_count_d(x@pointer, values),
    string = multiset_count_s(x@pointer, values),
    boolean = multiset_count_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppUnorderedMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_multiset_count_i(x@pointer, values),
    double = unordered_multiset_count_d(x@pointer, values),
    string = unordered_multiset_count_s(x@pointer, values),
    boolean = unordered_multiset_count_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppMap"), function(x, values) {
  # The values parameter refers to the keys and is called values for compliance with the generic method
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_count_i_i(x@pointer, values),
      double = map_count_i_d(x@pointer, values),
      string = map_count_i_s(x@pointer, values),
      boolean = map_count_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = map_count_d_i(x@pointer, values),
      double = map_count_d_d(x@pointer, values),
      string = map_count_d_s(x@pointer, values),
      boolean = map_count_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = map_count_s_i(x@pointer, values),
      double = map_count_s_d(x@pointer, values),
      string = map_count_s_s(x@pointer, values),
      boolean = map_count_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = map_count_b_i(x@pointer, values),
      double = map_count_b_d(x@pointer, values),
      string = map_count_b_s(x@pointer, values),
      boolean = map_count_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppUnorderedMap"), function(x, values) {
  # The values parameter refers to the keys and is called values for compliance with the generic method
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_count_i_i(x@pointer, values),
      double = unordered_map_count_i_d(x@pointer, values),
      string = unordered_map_count_i_s(x@pointer, values),
      boolean = unordered_map_count_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_map_count_d_i(x@pointer, values),
      double = unordered_map_count_d_d(x@pointer, values),
      string = unordered_map_count_d_s(x@pointer, values),
      boolean = unordered_map_count_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_map_count_s_i(x@pointer, values),
      double = unordered_map_count_s_d(x@pointer, values),
      string = unordered_map_count_s_s(x@pointer, values),
      boolean = unordered_map_count_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_map_count_b_i(x@pointer, values),
      double = unordered_map_count_b_d(x@pointer, values),
      string = unordered_map_count_b_s(x@pointer, values),
      boolean = unordered_map_count_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppMultimap"), function(x, values) {
  # The values parameter refers to the keys and is called values for compliance with the generic method
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_count_i_i(x@pointer, values),
      double = multimap_count_i_d(x@pointer, values),
      string = multimap_count_i_s(x@pointer, values),
      boolean = multimap_count_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = multimap_count_d_i(x@pointer, values),
      double = multimap_count_d_d(x@pointer, values),
      string = multimap_count_d_s(x@pointer, values),
      boolean = multimap_count_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = multimap_count_s_i(x@pointer, values),
      double = multimap_count_s_d(x@pointer, values),
      string = multimap_count_s_s(x@pointer, values),
      boolean = multimap_count_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = multimap_count_b_i(x@pointer, values),
      double = multimap_count_b_d(x@pointer, values),
      string = multimap_count_b_s(x@pointer, values),
      boolean = multimap_count_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("count", methods::signature(x = "CppUnorderedMultimap"), function(x, values) {
  # The values parameter refers to the keys and is called values for compliance with the generic method
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_count_i_i(x@pointer, values),
      double = unordered_multimap_count_i_d(x@pointer, values),
      string = unordered_multimap_count_i_s(x@pointer, values),
      boolean = unordered_multimap_count_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_multimap_count_d_i(x@pointer, values),
      double = unordered_multimap_count_d_d(x@pointer, values),
      string = unordered_multimap_count_d_s(x@pointer, values),
      boolean = unordered_multimap_count_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_multimap_count_s_i(x@pointer, values),
      double = unordered_multimap_count_s_d(x@pointer, values),
      string = unordered_multimap_count_s_s(x@pointer, values),
      boolean = unordered_multimap_count_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_count_b_i(x@pointer, values),
      double = unordered_multimap_count_b_d(x@pointer, values),
      string = unordered_multimap_count_b_s(x@pointer, values),
      boolean = unordered_multimap_count_b_b(x@pointer, values))
  ))
})
