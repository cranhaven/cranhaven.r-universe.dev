#' @include classes.R

methods::setMethod("show", methods::signature(object = "CppSet"), function(object) {
  return(switch(object@type,
    integer = set_show_i(object@pointer),
    double = set_show_d(object@pointer),
    string = set_show_s(object@pointer),
    boolean = set_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppUnorderedSet"), function(object) {
  return(switch(object@type,
    integer = unordered_set_show_i(object@pointer),
    double = unordered_set_show_d(object@pointer),
    string = unordered_set_show_s(object@pointer),
    boolean = unordered_set_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppMultiset"), function(object) {
  return(switch(object@type,
    integer = multiset_show_i(object@pointer),
    double = multiset_show_d(object@pointer),
    string = multiset_show_s(object@pointer),
    boolean = multiset_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppUnorderedMultiset"), function(object) {
  return(switch(object@type,
    integer = unordered_multiset_show_i(object@pointer),
    double = unordered_multiset_show_d(object@pointer),
    string = unordered_multiset_show_s(object@pointer),
    boolean = unordered_multiset_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppMap"), function(object) {
  return(switch(object@key_type,
    integer = switch(object@value_type,
      integer = map_show_i_i(object@pointer),
      double = map_show_i_d(object@pointer),
      string = map_show_i_s(object@pointer),
      boolean = map_show_i_b(object@pointer)),
    double = switch(object@value_type,
      integer = map_show_d_i(object@pointer),
      double = map_show_d_d(object@pointer),
      string = map_show_d_s(object@pointer),
      boolean = map_show_d_b(object@pointer)),
    string = switch(object@value_type,
      integer = map_show_s_i(object@pointer),
      double = map_show_s_d(object@pointer),
      string = map_show_s_s(object@pointer),
      boolean = map_show_s_b(object@pointer)),
    boolean = switch(object@value_type,
      integer = map_show_b_i(object@pointer),
      double = map_show_b_d(object@pointer),
      string = map_show_b_s(object@pointer),
      boolean = map_show_b_b(object@pointer))
  ))
})

methods::setMethod("show", methods::signature(object = "CppUnorderedMap"), function(object) {
  return(switch(object@key_type,
    integer = switch(object@value_type,
      integer = unordered_map_show_i_i(object@pointer),
      double = unordered_map_show_i_d(object@pointer),
      string = unordered_map_show_i_s(object@pointer),
      boolean = unordered_map_show_i_b(object@pointer)),
    double = switch(object@value_type,
      integer = unordered_map_show_d_i(object@pointer),
      double = unordered_map_show_d_d(object@pointer),
      string = unordered_map_show_d_s(object@pointer),
      boolean = unordered_map_show_d_b(object@pointer)),
    string = switch(object@value_type,
      integer = unordered_map_show_s_i(object@pointer),
      double = unordered_map_show_s_d(object@pointer),
      string = unordered_map_show_s_s(object@pointer),
      boolean = unordered_map_show_s_b(object@pointer)),
    boolean = switch(object@value_type,
      integer = unordered_map_show_b_i(object@pointer),
      double = unordered_map_show_b_d(object@pointer),
      string = unordered_map_show_b_s(object@pointer),
      boolean = unordered_map_show_b_b(object@pointer))
  ))
})

methods::setMethod("show", methods::signature(object = "CppMultimap"), function(object) {
  return(switch(object@key_type,
    integer = switch(object@value_type,
      integer = multimap_show_i_i(object@pointer),
      double = multimap_show_i_d(object@pointer),
      string = multimap_show_i_s(object@pointer),
      boolean = multimap_show_i_b(object@pointer)),
    double = switch(object@value_type,
      integer = multimap_show_d_i(object@pointer),
      double = multimap_show_d_d(object@pointer),
      string = multimap_show_d_s(object@pointer),
      boolean = multimap_show_d_b(object@pointer)),
    string = switch(object@value_type,
      integer = multimap_show_s_i(object@pointer),
      double = multimap_show_s_d(object@pointer),
      string = multimap_show_s_s(object@pointer),
      boolean = multimap_show_s_b(object@pointer)),
    boolean = switch(object@value_type,
      integer = multimap_show_b_i(object@pointer),
      double = multimap_show_b_d(object@pointer),
      string = multimap_show_b_s(object@pointer),
      boolean = multimap_show_b_b(object@pointer))
  ))
})

methods::setMethod("show", methods::signature(object = "CppUnorderedMultimap"), function(object) {
  return(switch(object@key_type,
    integer = switch(object@value_type,
      integer = unordered_multimap_show_i_i(object@pointer),
      double = unordered_multimap_show_i_d(object@pointer),
      string = unordered_multimap_show_i_s(object@pointer),
      boolean = unordered_multimap_show_i_b(object@pointer)),
    double = switch(object@value_type,
      integer = unordered_multimap_show_d_i(object@pointer),
      double = unordered_multimap_show_d_d(object@pointer),
      string = unordered_multimap_show_d_s(object@pointer),
      boolean = unordered_multimap_show_d_b(object@pointer)),
    string = switch(object@value_type,
      integer = unordered_multimap_show_s_i(object@pointer),
      double = unordered_multimap_show_s_d(object@pointer),
      string = unordered_multimap_show_s_s(object@pointer),
      boolean = unordered_multimap_show_s_b(object@pointer)),
    boolean = switch(object@value_type,
      integer = unordered_multimap_show_b_i(object@pointer),
      double = unordered_multimap_show_b_d(object@pointer),
      string = unordered_multimap_show_b_s(object@pointer),
      boolean = unordered_multimap_show_b_b(object@pointer))
  ))
})

methods::setMethod("show", methods::signature(object = "CppStack"), function(object) {
  return(switch(object@type,
    integer = stack_show_i(object@pointer),
    double = stack_show_d(object@pointer),
    string = stack_show_s(object@pointer),
    boolean = stack_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppQueue"), function(object) {
  return(switch(object@type,
    integer = queue_show_i(object@pointer),
    double = queue_show_d(object@pointer),
    string = queue_show_s(object@pointer),
    boolean = queue_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppPriorityQueue"), function(object) {
  if(object@ascending) {
    return(switch(object@type,
      integer = priority_queue_show_i_a(object@pointer),
      double = priority_queue_show_d_a(object@pointer),
      string = priority_queue_show_s_a(object@pointer),
      boolean = priority_queue_show_b_a(object@pointer)
    ))
  } else {
    return(switch(object@type,
      integer = priority_queue_show_i_d(object@pointer),
      double = priority_queue_show_d_d(object@pointer),
      string = priority_queue_show_s_d(object@pointer),
      boolean = priority_queue_show_b_d(object@pointer)
    ))
  }
})

methods::setMethod("show", methods::signature(object = "CppVector"), function(object) {
  return(switch(object@type,
    integer = vector_show_i(object@pointer),
    double = vector_show_d(object@pointer),
    string = vector_show_s(object@pointer),
    boolean = vector_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppDeque"), function(object) {
  return(switch(object@type,
    integer = deque_show_i(object@pointer),
    double = deque_show_d(object@pointer),
    string = deque_show_s(object@pointer),
    boolean = deque_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppForwardList"), function(object) {
  return(switch(object@type,
    integer = forward_list_show_i(object@pointer),
    double = forward_list_show_d(object@pointer),
    string = forward_list_show_s(object@pointer),
    boolean = forward_list_show_b(object@pointer)
  ))
})

methods::setMethod("show", methods::signature(object = "CppList"), function(object) {
  return(switch(object@type,
    integer = list_show_i(object@pointer),
    double = list_show_d(object@pointer),
    string = list_show_s(object@pointer),
    boolean = list_show_b(object@pointer)
  ))
})
