#' Get container size
#' 
#' Obtain the number of elements in a container.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppStack, CppQueue, 
#' CppPriorityQueue, CppVector, CppDeque, or CppList object.
#' 
#' @returns Returns a numeric.
#' 
#' @seealso \link{bucket_count}, \link{capacity}, \link{load_factor}, \link{max_size}, \link{resize}.
#' 
#' @examples
#' s <- cpp_unordered_set(4:6)
#' s
#' # 6 5 4
#' 
#' size(s)
#' # [1] 3
#' 

#' @aliases size,CppSet-method size,CppUnorderedSet-method size,CppMultiset-method size,CppUnorderedMultiset-method size,CppMap-method 
#' size,CppUnorderedMap-method size,CppMultimap-method size,CppUnorderedMultimap-method size,CppStack-method size,CppQueue-method 
#' size,CppPriorityQueue-method size,CppVector-method size,CppDeque-method size,CppList-method

#' @export
methods::setGeneric("size", function(x) standardGeneric("size"))

#' @include classes.R

#' @export
methods::setMethod("size", methods::signature(x = "CppSet"), function(x) {
  return(switch(x@type,
    integer = set_size_i(x@pointer),
    double = set_size_d(x@pointer),
    string = set_size_s(x@pointer),
    boolean = set_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_size_i(x@pointer),
    double = unordered_set_size_d(x@pointer),
    string = unordered_set_size_s(x@pointer),
    boolean = unordered_set_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppMultiset"), function(x) {
  return(switch(x@type,
    integer = multiset_size_i(x@pointer),
    double = multiset_size_d(x@pointer),
    string = multiset_size_s(x@pointer),
    boolean = multiset_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_size_i(x@pointer),
    double = unordered_multiset_size_d(x@pointer),
    string = unordered_multiset_size_s(x@pointer),
    boolean = unordered_multiset_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_size_i_i(x@pointer),
      double = map_size_i_d(x@pointer),
      string = map_size_i_s(x@pointer),
      boolean = map_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = map_size_d_i(x@pointer),
      double = map_size_d_d(x@pointer),
      string = map_size_d_s(x@pointer),
      boolean = map_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = map_size_s_i(x@pointer),
      double = map_size_s_d(x@pointer),
      string = map_size_s_s(x@pointer),
      boolean = map_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = map_size_b_i(x@pointer),
      double = map_size_b_d(x@pointer),
      string = map_size_b_s(x@pointer),
      boolean = map_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_size_i_i(x@pointer),
      double = unordered_map_size_i_d(x@pointer),
      string = unordered_map_size_i_s(x@pointer),
      boolean = unordered_map_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_size_d_i(x@pointer),
      double = unordered_map_size_d_d(x@pointer),
      string = unordered_map_size_d_s(x@pointer),
      boolean = unordered_map_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_size_s_i(x@pointer),
      double = unordered_map_size_s_d(x@pointer),
      string = unordered_map_size_s_s(x@pointer),
      boolean = unordered_map_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_size_b_i(x@pointer),
      double = unordered_map_size_b_d(x@pointer),
      string = unordered_map_size_b_s(x@pointer),
      boolean = unordered_map_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_size_i_i(x@pointer),
      double = multimap_size_i_d(x@pointer),
      string = multimap_size_i_s(x@pointer),
      boolean = multimap_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = multimap_size_d_i(x@pointer),
      double = multimap_size_d_d(x@pointer),
      string = multimap_size_d_s(x@pointer),
      boolean = multimap_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = multimap_size_s_i(x@pointer),
      double = multimap_size_s_d(x@pointer),
      string = multimap_size_s_s(x@pointer),
      boolean = multimap_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = multimap_size_b_i(x@pointer),
      double = multimap_size_b_d(x@pointer),
      string = multimap_size_b_s(x@pointer),
      boolean = multimap_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_size_i_i(x@pointer),
      double = unordered_multimap_size_i_d(x@pointer),
      string = unordered_multimap_size_i_s(x@pointer),
      boolean = unordered_multimap_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_size_d_i(x@pointer),
      double = unordered_multimap_size_d_d(x@pointer),
      string = unordered_multimap_size_d_s(x@pointer),
      boolean = unordered_multimap_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_size_s_i(x@pointer),
      double = unordered_multimap_size_s_d(x@pointer),
      string = unordered_multimap_size_s_s(x@pointer),
      boolean = unordered_multimap_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_size_b_i(x@pointer),
      double = unordered_multimap_size_b_d(x@pointer),
      string = unordered_multimap_size_b_s(x@pointer),
      boolean = unordered_multimap_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppStack"), function(x) {
  return(switch(x@type,
    integer = stack_size_i(x@pointer),
    double = stack_size_d(x@pointer),
    string = stack_size_s(x@pointer),
    boolean = stack_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppQueue"), function(x) {
  return(switch(x@type,
    integer = queue_size_i(x@pointer),
    double = queue_size_d(x@pointer),
    string = queue_size_s(x@pointer),
    boolean = queue_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppPriorityQueue"), function(x) {
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_size_i_a(x@pointer),
      double = priority_queue_size_d_a(x@pointer),
      string = priority_queue_size_s_a(x@pointer),
      boolean = priority_queue_size_b_a(x@pointer)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_size_i_d(x@pointer),
      double = priority_queue_size_d_d(x@pointer),
      string = priority_queue_size_s_d(x@pointer),
      boolean = priority_queue_size_b_d(x@pointer)
    ))
  }
})

#' @export
methods::setMethod("size", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_size_i(x@pointer),
    double = vector_size_d(x@pointer),
    string = vector_size_s(x@pointer),
    boolean = vector_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_size_i(x@pointer),
    double = deque_size_d(x@pointer),
    string = deque_size_s(x@pointer),
    boolean = deque_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("size", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_size_i(x@pointer),
    double = list_size_d(x@pointer),
    string = list_size_s(x@pointer),
    boolean = list_size_b(x@pointer)
  ))
})
