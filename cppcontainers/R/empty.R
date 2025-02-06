#' Check emptiness
#' 
#' Check, if a container is empty, i.e. does not contain elements.
#' 
#' @param x A \code{cppcontainers} object.
#' 
#' @returns Returns \code{TRUE}, if the container is empty, and \code{FALSE} otherwise.
#' 
#' @examples
#' v <- cpp_vector(4:6)
#' v
#' # 4 5 6
#' 
#' clear(v)
#' empty(v)
#' # [1] TRUE
#' 

#' @aliases empty,CppSet-method empty,CppUnorderedSet-method empty,CppMultiset-method empty,CppUnorderedMultiset-method empty,CppMap-method 
#' empty,CppUnorderedMap-method empty,CppMultimap-method empty,CppUnorderedMultimap-method empty,CppStack-method empty,CppQueue-method 
#' empty,CppPriorityQueue-method empty,CppVector-method empty,CppDeque-method empty,CppForwardList-method empty,CppList-method

#' @export
methods::setGeneric("empty", function(x) standardGeneric("empty"))

#' @include classes.R

#' @export
methods::setMethod("empty", methods::signature(x = "CppSet"), function(x) {
  return(switch(x@type,
    integer = set_empty_i(x@pointer),
    double = set_empty_d(x@pointer),
    string = set_empty_s(x@pointer),
    boolean = set_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_empty_i(x@pointer),
    double = unordered_set_empty_d(x@pointer),
    string = unordered_set_empty_s(x@pointer),
    boolean = unordered_set_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppMultiset"), function(x) {
  return(switch(x@type,
    integer = multiset_empty_i(x@pointer),
    double = multiset_empty_d(x@pointer),
    string = multiset_empty_s(x@pointer),
    boolean = multiset_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_empty_i(x@pointer),
    double = unordered_multiset_empty_d(x@pointer),
    string = unordered_multiset_empty_s(x@pointer),
    boolean = unordered_multiset_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_empty_i_i(x@pointer),
      double = map_empty_i_d(x@pointer),
      string = map_empty_i_s(x@pointer),
      boolean = map_empty_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = map_empty_d_i(x@pointer),
      double = map_empty_d_d(x@pointer),
      string = map_empty_d_s(x@pointer),
      boolean = map_empty_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = map_empty_s_i(x@pointer),
      double = map_empty_s_d(x@pointer),
      string = map_empty_s_s(x@pointer),
      boolean = map_empty_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = map_empty_b_i(x@pointer),
      double = map_empty_b_d(x@pointer),
      string = map_empty_b_s(x@pointer),
      boolean = map_empty_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_empty_i_i(x@pointer),
      double = unordered_map_empty_i_d(x@pointer),
      string = unordered_map_empty_i_s(x@pointer),
      boolean = unordered_map_empty_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_empty_d_i(x@pointer),
      double = unordered_map_empty_d_d(x@pointer),
      string = unordered_map_empty_d_s(x@pointer),
      boolean = unordered_map_empty_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_empty_s_i(x@pointer),
      double = unordered_map_empty_s_d(x@pointer),
      string = unordered_map_empty_s_s(x@pointer),
      boolean = unordered_map_empty_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_empty_b_i(x@pointer),
      double = unordered_map_empty_b_d(x@pointer),
      string = unordered_map_empty_b_s(x@pointer),
      boolean = unordered_map_empty_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_empty_i_i(x@pointer),
      double = multimap_empty_i_d(x@pointer),
      string = multimap_empty_i_s(x@pointer),
      boolean = multimap_empty_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = multimap_empty_d_i(x@pointer),
      double = multimap_empty_d_d(x@pointer),
      string = multimap_empty_d_s(x@pointer),
      boolean = multimap_empty_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = multimap_empty_s_i(x@pointer),
      double = multimap_empty_s_d(x@pointer),
      string = multimap_empty_s_s(x@pointer),
      boolean = multimap_empty_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = multimap_empty_b_i(x@pointer),
      double = multimap_empty_b_d(x@pointer),
      string = multimap_empty_b_s(x@pointer),
      boolean = multimap_empty_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_empty_i_i(x@pointer),
      double = unordered_multimap_empty_i_d(x@pointer),
      string = unordered_multimap_empty_i_s(x@pointer),
      boolean = unordered_multimap_empty_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_empty_d_i(x@pointer),
      double = unordered_multimap_empty_d_d(x@pointer),
      string = unordered_multimap_empty_d_s(x@pointer),
      boolean = unordered_multimap_empty_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_empty_s_i(x@pointer),
      double = unordered_multimap_empty_s_d(x@pointer),
      string = unordered_multimap_empty_s_s(x@pointer),
      boolean = unordered_multimap_empty_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_empty_b_i(x@pointer),
      double = unordered_multimap_empty_b_d(x@pointer),
      string = unordered_multimap_empty_b_s(x@pointer),
      boolean = unordered_multimap_empty_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppStack"), function(x) {
  return(switch(x@type,
    integer = stack_empty_i(x@pointer),
    double = stack_empty_d(x@pointer),
    string = stack_empty_s(x@pointer),
    boolean = stack_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppQueue"), function(x) {
  return(switch(x@type,
    integer = queue_empty_i(x@pointer),
    double = queue_empty_d(x@pointer),
    string = queue_empty_s(x@pointer),
    boolean = queue_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppPriorityQueue"), function(x) {
  if(x@ascending) {
    return(switch(x@type,
      integer = priority_queue_empty_i_a(x@pointer),
      double = priority_queue_empty_d_a(x@pointer),
      string = priority_queue_empty_s_a(x@pointer),
      boolean = priority_queue_empty_b_a(x@pointer)
    ))
  } else {
    return(switch(x@type,
      integer = priority_queue_empty_i_d(x@pointer),
      double = priority_queue_empty_d_d(x@pointer),
      string = priority_queue_empty_s_d(x@pointer),
      boolean = priority_queue_empty_b_d(x@pointer)
    ))
  }
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_empty_i(x@pointer),
    double = vector_empty_d(x@pointer),
    string = vector_empty_s(x@pointer),
    boolean = vector_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_empty_i(x@pointer),
    double = deque_empty_d(x@pointer),
    string = deque_empty_s(x@pointer),
    boolean = deque_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_empty_i(x@pointer),
    double = forward_list_empty_d(x@pointer),
    string = forward_list_empty_s(x@pointer),
    boolean = forward_list_empty_b(x@pointer)
  ))
})

#' @export
methods::setMethod("empty", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_empty_i(x@pointer),
    double = list_empty_d(x@pointer),
    string = list_empty_s(x@pointer),
    boolean = list_empty_b(x@pointer)
  ))
})
