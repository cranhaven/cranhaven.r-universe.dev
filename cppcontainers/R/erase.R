#' Erase elements
#' 
#' Delete elements from a container by reference.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppVector, CppDeque, 
#' or CppList object.
#' @param values A vector of values to delete from \code{x} in CppSet, CppUnorderedSet, CppMultiset, and CppUnorderedMultiset objects and keys in CppMap, 
#' CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects. Ignored for other classes.
#' @param from Index of the first element to be deleted in CppVector, CppDeque, and CppList objects. Ignored for other classes.
#' @param to Index of the last element to be deleted in CppVector, CppDeque, and CppList objects. Ignored for other classes.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{clear}, \link{empty}, \link{erase_after}, \link{remove.}.
#' 
#' @examples
#' s <- cpp_multiset(c(2, 2.1, 3, 3, 4.3, 6))
#' s
#' # 2 2.1 3 3 4.3 6
#' 
#' erase(s, c(2, 3))
#' s
#' # 2.1 4.3 6
#' 
#' m <- cpp_unordered_multimap(c(2:3, 3L), c("hello", "there", "world"))
#' m
#' # [3,"world"] [3,"there"] [2,"hello"]
#' 
#' erase(m, 2L)
#' m
#' # [3,"world"] [3,"there"]
#' 
#' d <- cpp_deque(4:9)
#' d
#' # 4 5 6 7 8 9
#' 
#' erase(d, from = 2, to = 3)
#' d
#' # 4 7 8 9
#' 

#' @aliases erase,CppSet-method erase,CppUnorderedSet-method erase,CppMultiset-method erase,CppUnorderedMultiset-method erase,CppMap-method 
#' erase,CppUnorderedMap-method erase,CppMultimap-method erase,CppUnorderedMultimap-method erase,CppVector-method erase,CppDeque-method erase,CppList-method

#' @export
methods::setGeneric("erase", function(x, values = NULL, from = NULL, to = NULL) standardGeneric("erase"))

#' @include classes.R

#' @export
methods::setMethod("erase", methods::signature(x = "CppSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = set_erase_i(x@pointer, values),
    double = set_erase_d(x@pointer, values),
    string = set_erase_s(x@pointer, values),
    boolean = set_erase_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppUnorderedSet"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_set_erase_i(x@pointer, values),
    double = unordered_set_erase_d(x@pointer, values),
    string = unordered_set_erase_s(x@pointer, values),
    boolean = unordered_set_erase_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = multiset_erase_i(x@pointer, values),
    double = multiset_erase_d(x@pointer, values),
    string = multiset_erase_s(x@pointer, values),
    boolean = multiset_erase_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppUnorderedMultiset"), function(x, values) {
  check_insert_values(values)
  return(switch(x@type,
    integer = unordered_multiset_erase_i(x@pointer, values),
    double = unordered_multiset_erase_d(x@pointer, values),
    string = unordered_multiset_erase_s(x@pointer, values),
    boolean = unordered_multiset_erase_b(x@pointer, values)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppMap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_erase_i_i(x@pointer, values),
      double = map_erase_i_d(x@pointer, values),
      string = map_erase_i_s(x@pointer, values),
      boolean = map_erase_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = map_erase_d_i(x@pointer, values),
      double = map_erase_d_d(x@pointer, values),
      string = map_erase_d_s(x@pointer, values),
      boolean = map_erase_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = map_erase_s_i(x@pointer, values),
      double = map_erase_s_d(x@pointer, values),
      string = map_erase_s_s(x@pointer, values),
      boolean = map_erase_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = map_erase_b_i(x@pointer, values),
      double = map_erase_b_d(x@pointer, values),
      string = map_erase_b_s(x@pointer, values),
      boolean = map_erase_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppUnorderedMap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_erase_i_i(x@pointer, values),
      double = unordered_map_erase_i_d(x@pointer, values),
      string = unordered_map_erase_i_s(x@pointer, values),
      boolean = unordered_map_erase_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_map_erase_d_i(x@pointer, values),
      double = unordered_map_erase_d_d(x@pointer, values),
      string = unordered_map_erase_d_s(x@pointer, values),
      boolean = unordered_map_erase_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_map_erase_s_i(x@pointer, values),
      double = unordered_map_erase_s_d(x@pointer, values),
      string = unordered_map_erase_s_s(x@pointer, values),
      boolean = unordered_map_erase_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_map_erase_b_i(x@pointer, values),
      double = unordered_map_erase_b_d(x@pointer, values),
      string = unordered_map_erase_b_s(x@pointer, values),
      boolean = unordered_map_erase_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppMultimap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_erase_i_i(x@pointer, values),
      double = multimap_erase_i_d(x@pointer, values),
      string = multimap_erase_i_s(x@pointer, values),
      boolean = multimap_erase_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = multimap_erase_d_i(x@pointer, values),
      double = multimap_erase_d_d(x@pointer, values),
      string = multimap_erase_d_s(x@pointer, values),
      boolean = multimap_erase_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = multimap_erase_s_i(x@pointer, values),
      double = multimap_erase_s_d(x@pointer, values),
      string = multimap_erase_s_s(x@pointer, values),
      boolean = multimap_erase_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = multimap_erase_b_i(x@pointer, values),
      double = multimap_erase_b_d(x@pointer, values),
      string = multimap_erase_b_s(x@pointer, values),
      boolean = multimap_erase_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppUnorderedMultimap"), function(x, values) {
  check_insert_values(values)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_erase_i_i(x@pointer, values),
      double = unordered_multimap_erase_i_d(x@pointer, values),
      string = unordered_multimap_erase_i_s(x@pointer, values),
      boolean = unordered_multimap_erase_i_b(x@pointer, values)),
    double = switch(x@value_type,
      integer = unordered_multimap_erase_d_i(x@pointer, values),
      double = unordered_multimap_erase_d_d(x@pointer, values),
      string = unordered_multimap_erase_d_s(x@pointer, values),
      boolean = unordered_multimap_erase_d_b(x@pointer, values)),
    string = switch(x@value_type,
      integer = unordered_multimap_erase_s_i(x@pointer, values),
      double = unordered_multimap_erase_s_d(x@pointer, values),
      string = unordered_multimap_erase_s_s(x@pointer, values),
      boolean = unordered_multimap_erase_s_b(x@pointer, values)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_erase_b_i(x@pointer, values),
      double = unordered_multimap_erase_b_d(x@pointer, values),
      string = unordered_multimap_erase_b_s(x@pointer, values),
      boolean = unordered_multimap_erase_b_b(x@pointer, values))
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppVector"), function(x, from, to) {
  return(switch(x@type,
    integer = vector_erase_i(x@pointer, from, to),
    double = vector_erase_d(x@pointer, from, to),
    string = vector_erase_s(x@pointer, from, to),
    boolean = vector_erase_b(x@pointer, from, to)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppDeque"), function(x, from, to) {
  return(switch(x@type,
    integer = deque_erase_i(x@pointer, from, to),
    double = deque_erase_d(x@pointer, from, to),
    string = deque_erase_s(x@pointer, from, to),
    boolean = deque_erase_b(x@pointer, from, to)
  ))
})

#' @export
methods::setMethod("erase", methods::signature(x = "CppList"), function(x, from, to) {
  return(switch(x@type,
    integer = list_erase_i(x@pointer, from, to),
    double = list_erase_d(x@pointer, from, to),
    string = list_erase_s(x@pointer, from, to),
    boolean = list_erase_b(x@pointer, from, to)
  ))
})
