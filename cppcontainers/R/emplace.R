#' Add an element
#' 
#' Add an element to a container by reference in place.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppStack, CppQueue, 
#' CppPriorityQueue, CppVector, CppDeque, or CppList object.
#' @param value A value to add to \code{x}.
#' @param key A key to add to \code{x}. Only relevant for CppMap, CppUnorderedMap, CppMultimap, and CppUnorderedMultimap objects.
#' @param position The index at which to add the element. Only relevant for CppVector, CppDeque, and CppList objects. Indices start at 1.
#' 
#' @details Existing container values are not overwritten. I.e. inserting a key-value pair into a map that already contains that key preserves the old 
#' value and discards the new one. Use \link{insert_or_assign} to overwrite values.
#' 
#' \code{emplace} and \link{try_emplace} produce the same results in the context of this package. \link{try_emplace} can be minimally more computationally 
#' efficient than \code{emplace}.
#' 
#' The emplace methods only add single elements. Use \link{insert} to add multiple elements in one call.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{assign}, \link{emplace_after}, \link{emplace_back}, \link{emplace_front}, \link{insert}, \link{try_emplace}.
#' 
#' @examples
#' s <- cpp_set(c(1.5, 2.3, 4.1))
#' s
#' # 1.5 2.3 4.1
#' 
#' emplace(s, 3.1)
#' s
#' # 1.5 2.3 3.1 4.1
#' 
#' m <- cpp_unordered_map(c("hello", "there"), c(TRUE, FALSE))
#' m
#' # ["there",FALSE] ["hello",TRUE]
#' 
#' emplace(m, TRUE, "world")
#' m
#' # ["world",TRUE] ["there",FALSE] ["hello",TRUE]
#' 
#' d <- cpp_deque(4:6)
#' d
#' # 4 5 6
#' 
#' emplace(d, 9, position = 2)
#' d
#' # 4 9 5 6
#' 

#' @aliases emplace,CppSet-method emplace,CppUnorderedSet-method emplace,CppMultiset-method emplace,CppUnorderedMultiset-method emplace,CppMap-method 
#' emplace,CppUnorderedMap-method emplace,CppMultimap-method emplace,CppUnorderedMultimap-method emplace,CppStack-method emplace,CppQueue-method 
#' emplace,CppPriorityQueue-method emplace,CppVector-method emplace,CppDeque-method emplace,CppList-method

#' @export
methods::setGeneric("emplace", function(x, value, key = NULL, position = NULL) standardGeneric("emplace"))

#' @include classes.R

#' @export
methods::setMethod("emplace", methods::signature(x = "CppSet"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = set_emplace_i(x@pointer, value),
    double = set_emplace_d(x@pointer, value),
    string = set_emplace_s(x@pointer, value),
    boolean = set_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppUnorderedSet"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = unordered_set_emplace_i(x@pointer, value),
    double = unordered_set_emplace_d(x@pointer, value),
    string = unordered_set_emplace_s(x@pointer, value),
    boolean = unordered_set_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppMultiset"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = multiset_emplace_i(x@pointer, value),
    double = multiset_emplace_d(x@pointer, value),
    string = multiset_emplace_s(x@pointer, value),
    boolean = multiset_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppUnorderedMultiset"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = unordered_multiset_emplace_i(x@pointer, value),
    double = unordered_multiset_emplace_d(x@pointer, value),
    string = unordered_multiset_emplace_s(x@pointer, value),
    boolean = unordered_multiset_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppMap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_emplace_i_i(x@pointer, key, value),
      double = map_emplace_i_d(x@pointer, key, value),
      string = map_emplace_i_s(x@pointer, key, value),
      boolean = map_emplace_i_b(x@pointer, key, value)),
    double = switch(x@value_type,
      integer = map_emplace_d_i(x@pointer, key, value),
      double = map_emplace_d_d(x@pointer, key, value),
      string = map_emplace_d_s(x@pointer, key, value),
      boolean = map_emplace_d_b(x@pointer, key, value)),
    string = switch(x@value_type,
      integer = map_emplace_s_i(x@pointer, key, value),
      double = map_emplace_s_d(x@pointer, key, value),
      string = map_emplace_s_s(x@pointer, key, value),
      boolean = map_emplace_s_b(x@pointer, key, value)),
    boolean = switch(x@value_type,
      integer = map_emplace_b_i(x@pointer, key, value),
      double = map_emplace_b_d(x@pointer, key, value),
      string = map_emplace_b_s(x@pointer, key, value),
      boolean = map_emplace_b_b(x@pointer, key, value))
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppUnorderedMap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_emplace_i_i(x@pointer, key, value),
      double = unordered_map_emplace_i_d(x@pointer, key, value),
      string = unordered_map_emplace_i_s(x@pointer, key, value),
      boolean = unordered_map_emplace_i_b(x@pointer, key, value)),
    double = switch(x@value_type,
      integer = unordered_map_emplace_d_i(x@pointer, key, value),
      double = unordered_map_emplace_d_d(x@pointer, key, value),
      string = unordered_map_emplace_d_s(x@pointer, key, value),
      boolean = unordered_map_emplace_d_b(x@pointer, key, value)),
    string = switch(x@value_type,
      integer = unordered_map_emplace_s_i(x@pointer, key, value),
      double = unordered_map_emplace_s_d(x@pointer, key, value),
      string = unordered_map_emplace_s_s(x@pointer, key, value),
      boolean = unordered_map_emplace_s_b(x@pointer, key, value)),
    boolean = switch(x@value_type,
      integer = unordered_map_emplace_b_i(x@pointer, key, value),
      double = unordered_map_emplace_b_d(x@pointer, key, value),
      string = unordered_map_emplace_b_s(x@pointer, key, value),
      boolean = unordered_map_emplace_b_b(x@pointer, key, value))
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppMultimap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_emplace_i_i(x@pointer, key, value),
      double = multimap_emplace_i_d(x@pointer, key, value),
      string = multimap_emplace_i_s(x@pointer, key, value),
      boolean = multimap_emplace_i_b(x@pointer, key, value)),
    double = switch(x@value_type,
      integer = multimap_emplace_d_i(x@pointer, key, value),
      double = multimap_emplace_d_d(x@pointer, key, value),
      string = multimap_emplace_d_s(x@pointer, key, value),
      boolean = multimap_emplace_d_b(x@pointer, key, value)),
    string = switch(x@value_type,
      integer = multimap_emplace_s_i(x@pointer, key, value),
      double = multimap_emplace_s_d(x@pointer, key, value),
      string = multimap_emplace_s_s(x@pointer, key, value),
      boolean = multimap_emplace_s_b(x@pointer, key, value)),
    boolean = switch(x@value_type,
      integer = multimap_emplace_b_i(x@pointer, key, value),
      double = multimap_emplace_b_d(x@pointer, key, value),
      string = multimap_emplace_b_s(x@pointer, key, value),
      boolean = multimap_emplace_b_b(x@pointer, key, value))
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppUnorderedMultimap"), function(x, value, key) {
  check_insert_value(key)
  check_insert_value(value)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_emplace_i_i(x@pointer, key, value),
      double = unordered_multimap_emplace_i_d(x@pointer, key, value),
      string = unordered_multimap_emplace_i_s(x@pointer, key, value),
      boolean = unordered_multimap_emplace_i_b(x@pointer, key, value)),
    double = switch(x@value_type,
      integer = unordered_multimap_emplace_d_i(x@pointer, key, value),
      double = unordered_multimap_emplace_d_d(x@pointer, key, value),
      string = unordered_multimap_emplace_d_s(x@pointer, key, value),
      boolean = unordered_multimap_emplace_d_b(x@pointer, key, value)),
    string = switch(x@value_type,
      integer = unordered_multimap_emplace_s_i(x@pointer, key, value),
      double = unordered_multimap_emplace_s_d(x@pointer, key, value),
      string = unordered_multimap_emplace_s_s(x@pointer, key, value),
      boolean = unordered_multimap_emplace_s_b(x@pointer, key, value)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_emplace_b_i(x@pointer, key, value),
      double = unordered_multimap_emplace_b_d(x@pointer, key, value),
      string = unordered_multimap_emplace_b_s(x@pointer, key, value),
      boolean = unordered_multimap_emplace_b_b(x@pointer, key, value))
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppStack"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = stack_emplace_i(x@pointer, value),
    double = stack_emplace_d(x@pointer, value),
    string = stack_emplace_s(x@pointer, value),
    boolean = stack_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppQueue"), function(x, value) {
  check_insert_value(value)
  return(switch(x@type,
    integer = queue_emplace_i(x@pointer, value),
    double = queue_emplace_d(x@pointer, value),
    string = queue_emplace_s(x@pointer, value),
    boolean = queue_emplace_b(x@pointer, value)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppPriorityQueue"), function(x, value) {
  check_insert_value(value)
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_emplace_i_a(x@pointer, value),
      double = priority_queue_emplace_d_a(x@pointer, value),
      string = priority_queue_emplace_s_a(x@pointer, value),
      boolean = priority_queue_emplace_b_a(x@pointer, value)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_emplace_i_d(x@pointer, value),
      double = priority_queue_emplace_d_d(x@pointer, value),
      string = priority_queue_emplace_s_d(x@pointer, value),
      boolean = priority_queue_emplace_b_d(x@pointer, value)
    ))
  }
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppVector"), function(x, value, position) {
  check_insert_value(value)
  position <- position - 1L
  return(switch(x@type,
    integer = vector_emplace_i(x@pointer, value, position),
    double = vector_emplace_d(x@pointer, value, position),
    string = vector_emplace_s(x@pointer, value, position),
    boolean = vector_emplace_b(x@pointer, value, position)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppDeque"), function(x, value, position) {
  check_insert_value(value)
  position <- position - 1L
  return(switch(x@type,
    integer = deque_emplace_i(x@pointer, value, position),
    double = deque_emplace_d(x@pointer, value, position),
    string = deque_emplace_s(x@pointer, value, position),
    boolean = deque_emplace_b(x@pointer, value, position)
  ))
})

#' @export
methods::setMethod("emplace", methods::signature(x = "CppList"), function(x, value, position) {
  check_insert_value(value)
  position <- position - 1L
  return(switch(x@type,
    integer = list_emplace_i(x@pointer, value, position),
    double = list_emplace_d(x@pointer, value, position),
    string = list_emplace_s(x@pointer, value, position),
    boolean = list_emplace_b(x@pointer, value, position)
  ))
})
