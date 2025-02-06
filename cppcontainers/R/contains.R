#' Check for elements
#' 
#' Check, if elements are part of a container.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, or CppUnorderedMultimap object.
#' @param values Values whose existence to assess. Refers to keys in the case of CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects.
#' 
#' @returns Returns a logical vector of the same length as \code{values}, denoting for each value whether it is part of \code{x} (\code{TRUE}) or not 
#' (\code{FALSE}).
#' 
#' @seealso \link{[}, \link{at}, \link{back}, \link{front}, \link{top}.
#' 
#' @examples
#' s <- cpp_multiset(4:9)
#' s
#' # 4 5 6 7 8 9
#' 
#' contains(s, 9:11)
#' # [1]  TRUE FALSE FALSE
#' 
#' m <- cpp_unordered_map(c("hello", "world"), 3:4)
#' m
#' # ["world",4] ["hello",3]
#' 
#' contains(m, c("world", "there"))
#' # [1]  TRUE FALSE
#' 

#' @aliases contains,CppSet-method contains,CppUnorderedSet-method contains,CppMultiset-method contains,CppUnorderedMultiset-method contains,CppMap-method 
#' contains,CppUnorderedMap-method contains,CppMultimap-method contains,CppUnorderedMultimap-method

#' @export
methods::setGeneric("contains", function(x, values) standardGeneric("contains"))

#' @include classes.R

#' @export
methods::setMethod("contains", methods::signature(x = "CppSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = set_contains_i(x@pointer, values),
    double = set_contains_d(x@pointer, values),
    string = set_contains_s(x@pointer, values),
    boolean = set_contains_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppUnorderedSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_set_contains_i(x@pointer, values),
    double = unordered_set_contains_d(x@pointer, values),
    string = unordered_set_contains_s(x@pointer, values),
    boolean = unordered_set_contains_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = multiset_contains_i(x@pointer, values),
    double = multiset_contains_d(x@pointer, values),
    string = multiset_contains_s(x@pointer, values),
    boolean = multiset_contains_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppUnorderedMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_multiset_contains_i(x@pointer, values),
    double = unordered_multiset_contains_d(x@pointer, values),
    string = unordered_multiset_contains_s(x@pointer, values),
    boolean = unordered_multiset_contains_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppMap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_contains_i_i(x@pointer, values),
      double = map_contains_i_d(x@pointer, values),
      string = map_contains_i_s(x@pointer, values),
      boolean = map_contains_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = map_contains_d_i(x@pointer, values),
      double = map_contains_d_d(x@pointer, values),
      string = map_contains_d_s(x@pointer, values),
      boolean = map_contains_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = map_contains_s_i(x@pointer, values),
      double = map_contains_s_d(x@pointer, values),
      string = map_contains_s_s(x@pointer, values),
      boolean = map_contains_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = map_contains_b_i(x@pointer, values),
      double = map_contains_b_d(x@pointer, values),
      string = map_contains_b_s(x@pointer, values),
      boolean = map_contains_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppUnorderedMap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_contains_i_i(x@pointer, values),
      double = unordered_map_contains_i_d(x@pointer, values),
      string = unordered_map_contains_i_s(x@pointer, values),
      boolean = unordered_map_contains_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_map_contains_d_i(x@pointer, values),
      double = unordered_map_contains_d_d(x@pointer, values),
      string = unordered_map_contains_d_s(x@pointer, values),
      boolean = unordered_map_contains_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_map_contains_s_i(x@pointer, values),
      double = unordered_map_contains_s_d(x@pointer, values),
      string = unordered_map_contains_s_s(x@pointer, values),
      boolean = unordered_map_contains_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_map_contains_b_i(x@pointer, values),
      double = unordered_map_contains_b_d(x@pointer, values),
      string = unordered_map_contains_b_s(x@pointer, values),
      boolean = unordered_map_contains_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppMultimap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_contains_i_i(x@pointer, values),
      double = multimap_contains_i_d(x@pointer, values),
      string = multimap_contains_i_s(x@pointer, values),
      boolean = multimap_contains_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = multimap_contains_d_i(x@pointer, values),
      double = multimap_contains_d_d(x@pointer, values),
      string = multimap_contains_d_s(x@pointer, values),
      boolean = multimap_contains_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = multimap_contains_s_i(x@pointer, values),
      double = multimap_contains_s_d(x@pointer, values),
      string = multimap_contains_s_s(x@pointer, values),
      boolean = multimap_contains_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = multimap_contains_b_i(x@pointer, values),
      double = multimap_contains_b_d(x@pointer, values),
      string = multimap_contains_b_s(x@pointer, values),
      boolean = multimap_contains_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("contains", methods::signature(x = "CppUnorderedMultimap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_contains_i_i(x@pointer, values),
      double = unordered_multimap_contains_i_d(x@pointer, values),
      string = unordered_multimap_contains_i_s(x@pointer, values),
      boolean = unordered_multimap_contains_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_multimap_contains_d_i(x@pointer, values),
      double = unordered_multimap_contains_d_d(x@pointer, values),
      string = unordered_multimap_contains_d_s(x@pointer, values),
      boolean = unordered_multimap_contains_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_multimap_contains_s_i(x@pointer, values),
      double = unordered_multimap_contains_s_d(x@pointer, values),
      string = unordered_multimap_contains_s_s(x@pointer, values),
      boolean = unordered_multimap_contains_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_contains_b_i(x@pointer, values),
      double = unordered_multimap_contains_b_d(x@pointer, values),
      string = unordered_multimap_contains_b_s(x@pointer, values),
      boolean = unordered_multimap_contains_b_b(x@pointer, values))
  ))
})
