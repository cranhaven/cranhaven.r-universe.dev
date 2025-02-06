#' Clear the container
#' 
#' Remove all elements from a container by reference.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppVector, CppDeque, 
#' CppForwardList, or CppList object.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{erase}, \link{remove.}, \link{empty}.
#' 
#' @examples
#' l <- cpp_forward_list(4:9)
#' l
#' # 4 5 6 7 8 9
#' 
#' clear(l)
#' l
#' # 
#' empty(l)
#' # [1] TRUE
#' 

#' @aliases clear,CppSet-method clear,CppUnorderedSet-method clear,CppMultiset-method clear,CppUnorderedMultiset-method clear,CppMap-method 
#' clear,CppUnorderedMap-method clear,CppMultimap-method clear,CppUnorderedMultimap-method clear,CppVector-method clear,CppDeque-method 
#' clear,CppForwardList-method clear,CppList-method

#' @export
methods::setGeneric("clear", function(x) standardGeneric("clear"))

#' @include classes.R

#' @export
methods::setMethod("clear", methods::signature(x = "CppSet"), function(x) {
  return(switch(x@type,
    integer = set_clear_i(x@pointer),
    double = set_clear_d(x@pointer),
    string = set_clear_s(x@pointer),
    boolean = set_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_clear_i(x@pointer),
    double = unordered_set_clear_d(x@pointer),
    string = unordered_set_clear_s(x@pointer),
    boolean = unordered_set_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppMultiset"), function(x) {
  return(switch(x@type,
    integer = multiset_clear_i(x@pointer),
    double = multiset_clear_d(x@pointer),
    string = multiset_clear_s(x@pointer),
    boolean = multiset_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_clear_i(x@pointer),
    double = unordered_multiset_clear_d(x@pointer),
    string = unordered_multiset_clear_s(x@pointer),
    boolean = unordered_multiset_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_clear_i_i(x@pointer),
      double = map_clear_i_d(x@pointer),
      string = map_clear_i_s(x@pointer),
      boolean = map_clear_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = map_clear_d_i(x@pointer),
      double = map_clear_d_d(x@pointer),
      string = map_clear_d_s(x@pointer),
      boolean = map_clear_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = map_clear_s_i(x@pointer),
      double = map_clear_s_d(x@pointer),
      string = map_clear_s_s(x@pointer),
      boolean = map_clear_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = map_clear_b_i(x@pointer),
      double = map_clear_b_d(x@pointer),
      string = map_clear_b_s(x@pointer),
      boolean = map_clear_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_clear_i_i(x@pointer),
      double = unordered_map_clear_i_d(x@pointer),
      string = unordered_map_clear_i_s(x@pointer),
      boolean = unordered_map_clear_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_clear_d_i(x@pointer),
      double = unordered_map_clear_d_d(x@pointer),
      string = unordered_map_clear_d_s(x@pointer),
      boolean = unordered_map_clear_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_clear_s_i(x@pointer),
      double = unordered_map_clear_s_d(x@pointer),
      string = unordered_map_clear_s_s(x@pointer),
      boolean = unordered_map_clear_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_clear_b_i(x@pointer),
      double = unordered_map_clear_b_d(x@pointer),
      string = unordered_map_clear_b_s(x@pointer),
      boolean = unordered_map_clear_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_clear_i_i(x@pointer),
      double = multimap_clear_i_d(x@pointer),
      string = multimap_clear_i_s(x@pointer),
      boolean = multimap_clear_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = multimap_clear_d_i(x@pointer),
      double = multimap_clear_d_d(x@pointer),
      string = multimap_clear_d_s(x@pointer),
      boolean = multimap_clear_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = multimap_clear_s_i(x@pointer),
      double = multimap_clear_s_d(x@pointer),
      string = multimap_clear_s_s(x@pointer),
      boolean = multimap_clear_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = multimap_clear_b_i(x@pointer),
      double = multimap_clear_b_d(x@pointer),
      string = multimap_clear_b_s(x@pointer),
      boolean = multimap_clear_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_clear_i_i(x@pointer),
      double = unordered_multimap_clear_i_d(x@pointer),
      string = unordered_multimap_clear_i_s(x@pointer),
      boolean = unordered_multimap_clear_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_clear_d_i(x@pointer),
      double = unordered_multimap_clear_d_d(x@pointer),
      string = unordered_multimap_clear_d_s(x@pointer),
      boolean = unordered_multimap_clear_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_clear_s_i(x@pointer),
      double = unordered_multimap_clear_s_d(x@pointer),
      string = unordered_multimap_clear_s_s(x@pointer),
      boolean = unordered_multimap_clear_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_clear_b_i(x@pointer),
      double = unordered_multimap_clear_b_d(x@pointer),
      string = unordered_multimap_clear_b_s(x@pointer),
      boolean = unordered_multimap_clear_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppVector"), function(x) {
  return(switch(x@type,
    integer = vector_clear_i(x@pointer),
    double = vector_clear_d(x@pointer),
    string = vector_clear_s(x@pointer),
    boolean = vector_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppDeque"), function(x) {
  return(switch(x@type,
    integer = deque_clear_i(x@pointer),
    double = deque_clear_d(x@pointer),
    string = deque_clear_s(x@pointer),
    boolean = deque_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppForwardList"), function(x) {
  return(switch(x@type,
    integer = forward_list_clear_i(x@pointer),
    double = forward_list_clear_d(x@pointer),
    string = forward_list_clear_s(x@pointer),
    boolean = forward_list_clear_b(x@pointer)
  ))
})

#' @export
methods::setMethod("clear", methods::signature(x = "CppList"), function(x) {
  return(switch(x@type,
    integer = list_clear_i(x@pointer),
    double = list_clear_d(x@pointer),
    string = list_clear_s(x@pointer),
    boolean = list_clear_b(x@pointer)
  ))
})
