#' Merge two objects
#' 
#' Merge two objects by reference.
#' 
#' @param x A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppForwardList, or 
#' CppList object.
#' @param y A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppForwardList, or 
#' CppList object of the same class and data type as \code{x}.
#' @param ... Ignored. Only included for compatibility with generic \code{base::merge} method.
#' 
#' @details In containers enforcing uniqueness (CppSet, CppUnorderedSet, CppMap, CppUnorderedMap), the function merges elements from \code{y} that are not 
#' in \code{x} into \code{x} and deletes them from \code{y}. In other container types, it transfers all elements.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{assign}, \link{emplace}, \link{insert}.
#' 
#' @examples
#' x <- cpp_set(c("hello", "there"))
#' y <- cpp_set(c("hello", "world"))
#' 
#' merge(x, y)
#' x
#' # "hello" "there" "world"
#' y
#' # "hello"
#' 
#' x <- cpp_forward_list(c(1, 3, 4, 3))
#' y <- cpp_forward_list(c(2, 3, 5))
#' 
#' merge(x, y)
#' x
#' # 1 2 3 3 4 3 5
#' y
#' # 
#' 

#' @aliases merge,CppSet,CppSet-method merge,CppUnorderedSet,CppUnorderedSet-method merge,CppMultiset,CppMultiset-method 
#' merge,CppUnorderedMultiset,CppUnorderedMultiset-method merge,CppMap,CppMap-method merge,CppUnorderedMap,CppUnorderedMap-method 
#' merge,CppMultimap,CppMultimap-method merge,CppUnorderedMultimap,CppUnorderedMultimap-method merge,CppForwardList,CppForwardList-method 
#' merge,CppList,CppList-method

methods::setGeneric("merge", methods::getGeneric("merge", package = "base"))

#' @include classes.R

#' @export
methods::setMethod("merge", methods::signature(x = "CppSet", y = "CppSet"), function(x, y) {
  return(switch(x@type,
    integer = set_merge_i(x@pointer, y@pointer),
    double = set_merge_d(x@pointer, y@pointer),
    string = set_merge_s(x@pointer, y@pointer),
    boolean = set_merge_b(x@pointer, y@pointer)
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppUnorderedSet", y = "CppUnorderedSet"), function(x, y) {
  return(switch(x@type,
    integer = unordered_set_merge_i(x@pointer, y@pointer),
    double = unordered_set_merge_d(x@pointer, y@pointer),
    string = unordered_set_merge_s(x@pointer, y@pointer),
    boolean = unordered_set_merge_b(x@pointer, y@pointer)
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppMultiset", y = "CppMultiset"), function(x, y) {
  return(switch(x@type,
    integer = multiset_merge_i(x@pointer, y@pointer),
    double = multiset_merge_d(x@pointer, y@pointer),
    string = multiset_merge_s(x@pointer, y@pointer),
    boolean = multiset_merge_b(x@pointer, y@pointer)
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppUnorderedMultiset", y = "CppUnorderedMultiset"), function(x, y) {
  return(switch(x@type,
    integer = unordered_multiset_merge_i(x@pointer, y@pointer),
    double = unordered_multiset_merge_d(x@pointer, y@pointer),
    string = unordered_multiset_merge_s(x@pointer, y@pointer),
    boolean = unordered_multiset_merge_b(x@pointer, y@pointer)
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppMap", y = "CppMap"), function(x, y) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_merge_i_i(x@pointer, y@pointer),
      double = map_merge_i_d(x@pointer, y@pointer),
      string = map_merge_i_s(x@pointer, y@pointer),
      boolean = map_merge_i_b(x@pointer, y@pointer)),
    double = switch(x@value_type,
      integer = map_merge_d_i(x@pointer, y@pointer),
      double = map_merge_d_d(x@pointer, y@pointer),
      string = map_merge_d_s(x@pointer, y@pointer),
      boolean = map_merge_d_b(x@pointer, y@pointer)),
    string = switch(x@value_type,
      integer = map_merge_s_i(x@pointer, y@pointer),
      double = map_merge_s_d(x@pointer, y@pointer),
      string = map_merge_s_s(x@pointer, y@pointer),
      boolean = map_merge_s_b(x@pointer, y@pointer)),
    boolean = switch(x@value_type,
      integer = map_merge_b_i(x@pointer, y@pointer),
      double = map_merge_b_d(x@pointer, y@pointer),
      string = map_merge_b_s(x@pointer, y@pointer),
      boolean = map_merge_b_b(x@pointer, y@pointer))
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppUnorderedMap", y = "CppUnorderedMap"), function(x, y) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_merge_i_i(x@pointer, y@pointer),
      double = unordered_map_merge_i_d(x@pointer, y@pointer),
      string = unordered_map_merge_i_s(x@pointer, y@pointer),
      boolean = unordered_map_merge_i_b(x@pointer, y@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_merge_d_i(x@pointer, y@pointer),
      double = unordered_map_merge_d_d(x@pointer, y@pointer),
      string = unordered_map_merge_d_s(x@pointer, y@pointer),
      boolean = unordered_map_merge_d_b(x@pointer, y@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_merge_s_i(x@pointer, y@pointer),
      double = unordered_map_merge_s_d(x@pointer, y@pointer),
      string = unordered_map_merge_s_s(x@pointer, y@pointer),
      boolean = unordered_map_merge_s_b(x@pointer, y@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_merge_b_i(x@pointer, y@pointer),
      double = unordered_map_merge_b_d(x@pointer, y@pointer),
      string = unordered_map_merge_b_s(x@pointer, y@pointer),
      boolean = unordered_map_merge_b_b(x@pointer, y@pointer))
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppMultimap", y = "CppMultimap"), function(x, y) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = multimap_merge_i_i(x@pointer, y@pointer),
      double = multimap_merge_i_d(x@pointer, y@pointer),
      string = multimap_merge_i_s(x@pointer, y@pointer),
      boolean = multimap_merge_i_b(x@pointer, y@pointer)),
    double = switch(x@value_type,
      integer = multimap_merge_d_i(x@pointer, y@pointer),
      double = multimap_merge_d_d(x@pointer, y@pointer),
      string = multimap_merge_d_s(x@pointer, y@pointer),
      boolean = multimap_merge_d_b(x@pointer, y@pointer)),
    string = switch(x@value_type,
      integer = multimap_merge_s_i(x@pointer, y@pointer),
      double = multimap_merge_s_d(x@pointer, y@pointer),
      string = multimap_merge_s_s(x@pointer, y@pointer),
      boolean = multimap_merge_s_b(x@pointer, y@pointer)),
    boolean = switch(x@value_type,
      integer = multimap_merge_b_i(x@pointer, y@pointer),
      double = multimap_merge_b_d(x@pointer, y@pointer),
      string = multimap_merge_b_s(x@pointer, y@pointer),
      boolean = multimap_merge_b_b(x@pointer, y@pointer))
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppUnorderedMultimap", y = "CppUnorderedMultimap"), function(x, y) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_merge_i_i(x@pointer, y@pointer),
      double = unordered_multimap_merge_i_d(x@pointer, y@pointer),
      string = unordered_multimap_merge_i_s(x@pointer, y@pointer),
      boolean = unordered_multimap_merge_i_b(x@pointer, y@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_merge_d_i(x@pointer, y@pointer),
      double = unordered_multimap_merge_d_d(x@pointer, y@pointer),
      string = unordered_multimap_merge_d_s(x@pointer, y@pointer),
      boolean = unordered_multimap_merge_d_b(x@pointer, y@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_merge_s_i(x@pointer, y@pointer),
      double = unordered_multimap_merge_s_d(x@pointer, y@pointer),
      string = unordered_multimap_merge_s_s(x@pointer, y@pointer),
      boolean = unordered_multimap_merge_s_b(x@pointer, y@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_merge_b_i(x@pointer, y@pointer),
      double = unordered_multimap_merge_b_d(x@pointer, y@pointer),
      string = unordered_multimap_merge_b_s(x@pointer, y@pointer),
      boolean = unordered_multimap_merge_b_b(x@pointer, y@pointer))
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppForwardList", y = "CppForwardList"), function(x, y) {
  return(switch(x@type,
    integer = forward_list_merge_i(x@pointer, y@pointer),
    double = forward_list_merge_d(x@pointer, y@pointer),
    string = forward_list_merge_s(x@pointer, y@pointer),
    boolean = forward_list_merge_b(x@pointer, y@pointer)
  ))
})

#' @export
methods::setMethod("merge", methods::signature(x = "CppList", y = "CppList"), function(x, y) {
  return(switch(x@type,
    integer = list_merge_i(x@pointer, y@pointer),
    double = list_merge_d(x@pointer, y@pointer),
    string = list_merge_s(x@pointer, y@pointer),
    boolean = list_merge_b(x@pointer, y@pointer)
  ))
})
