#' Get maximum container size
#' 
#' Obtain the maximum number of elements the container can hold.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppVector, CppDeque, 
#' CppForwardList, or CppList object.
#' 
#' @returns Returns a numeric.
#' 
#' @seealso \link{capacity}, \link{max_bucket_count}, \link{max_load_factor}, \link{size}.
#' 
#' @examples
#' s <- cpp_deque(4:6)
#' s
#' # 4 5 6
#' 
#' max_size(s)
#' # [1] 4.611686e+18
#' 

#' @aliases max_size,CppSet-method max_size,CppUnorderedSet-method max_size,CppMultiset-method max_size,CppUnorderedMultiset-method max_size,CppMap-method 
#' max_size,CppUnorderedMap-method max_size,CppMultimap-method max_size,CppUnorderedMultimap-method max_size,CppVector-method max_size,CppDeque-method 
#' max_size,CppForwardList-method max_size,CppList-method

#' @export
methods::setGeneric("max_size", function(x) standardGeneric("max_size"))

#' @include classes.R

#' @export
methods::setMethod("max_size", methods::signature(x = "CppSet"), function(x) {
  return(switch(x@type,
    integer = set_max_size_i(x@pointer),
    double = set_max_size_d(x@pointer),
    string = set_max_size_s(x@pointer),
    boolean = set_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_max_size_i(x@pointer),
    double = unordered_set_max_size_d(x@pointer),
    string = unordered_set_max_size_s(x@pointer),
    boolean = unordered_set_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppMultiset"), function(x) {
  return(switch(x@type,
    integer = multiset_max_size_i(x@pointer),
    double = multiset_max_size_d(x@pointer),
    string = multiset_max_size_s(x@pointer),
    boolean = multiset_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_max_size_i(x@pointer),
    double = unordered_multiset_max_size_d(x@pointer),
    string = unordered_multiset_max_size_s(x@pointer),
    boolean = unordered_multiset_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_max_size_i_i(x@pointer),
      double = map_max_size_i_d(x@pointer),
      string = map_max_size_i_s(x@pointer),
      boolean = map_max_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = map_max_size_d_i(x@pointer),
      double = map_max_size_d_d(x@pointer),
      string = map_max_size_d_s(x@pointer),
      boolean = map_max_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = map_max_size_s_i(x@pointer),
      double = map_max_size_s_d(x@pointer),
      string = map_max_size_s_s(x@pointer),
      boolean = map_max_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = map_max_size_b_i(x@pointer),
      double = map_max_size_b_d(x@pointer),
      string = map_max_size_b_s(x@pointer),
      boolean = map_max_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_max_size_i_i(x@pointer),
      double = unordered_map_max_size_i_d(x@pointer),
      string = unordered_map_max_size_i_s(x@pointer),
      boolean = unordered_map_max_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_max_size_d_i(x@pointer),
      double = unordered_map_max_size_d_d(x@pointer),
      string = unordered_map_max_size_d_s(x@pointer),
      boolean = unordered_map_max_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_max_size_s_i(x@pointer),
      double = unordered_map_max_size_s_d(x@pointer),
      string = unordered_map_max_size_s_s(x@pointer),
      boolean = unordered_map_max_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_max_size_b_i(x@pointer),
      double = unordered_map_max_size_b_d(x@pointer),
      string = unordered_map_max_size_b_s(x@pointer),
      boolean = unordered_map_max_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_max_size_i_i(x@pointer),
      double = multimap_max_size_i_d(x@pointer),
      string = multimap_max_size_i_s(x@pointer),
      boolean = multimap_max_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = multimap_max_size_d_i(x@pointer),
      double = multimap_max_size_d_d(x@pointer),
      string = multimap_max_size_d_s(x@pointer),
      boolean = multimap_max_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = multimap_max_size_s_i(x@pointer),
      double = multimap_max_size_s_d(x@pointer),
      string = multimap_max_size_s_s(x@pointer),
      boolean = multimap_max_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = multimap_max_size_b_i(x@pointer),
      double = multimap_max_size_b_d(x@pointer),
      string = multimap_max_size_b_s(x@pointer),
      boolean = multimap_max_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_max_size_i_i(x@pointer),
      double = unordered_multimap_max_size_i_d(x@pointer),
      string = unordered_multimap_max_size_i_s(x@pointer),
      boolean = unordered_multimap_max_size_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_max_size_d_i(x@pointer),
      double = unordered_multimap_max_size_d_d(x@pointer),
      string = unordered_multimap_max_size_d_s(x@pointer),
      boolean = unordered_multimap_max_size_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_max_size_s_i(x@pointer),
      double = unordered_multimap_max_size_s_d(x@pointer),
      string = unordered_multimap_max_size_s_s(x@pointer),
      boolean = unordered_multimap_max_size_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_max_size_b_i(x@pointer),
      double = unordered_multimap_max_size_b_d(x@pointer),
      string = unordered_multimap_max_size_b_s(x@pointer),
      boolean = unordered_multimap_max_size_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_max_size_i(x@pointer),
    double = vector_max_size_d(x@pointer),
    string = vector_max_size_s(x@pointer),
    boolean = vector_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_max_size_i(x@pointer),
    double = deque_max_size_d(x@pointer),
    string = deque_max_size_s(x@pointer),
    boolean = deque_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_max_size_i(x@pointer),
    double = forward_list_max_size_d(x@pointer),
    string = forward_list_max_size_s(x@pointer),
    boolean = forward_list_max_size_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_size", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_max_size_i(x@pointer),
    double = list_max_size_d(x@pointer),
    string = list_max_size_s(x@pointer),
    boolean = list_max_size_b(x@pointer)
  ))
})
