#' Check equality
#' 
#' Check, if two containers hold identical data.
#' 
#' @param e1 A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppStack, CppQueue, 
#' CppVector, CppDeque, CppForwardList, or CppList object.
#' @param e2 A CppSet, CppUnorderedSet, CppMultiset, CppUnorderedMultiset, CppMap, CppUnorderedMap, CppMultimap, CppUnorderedMultimap, CppStack, CppQueue, 
#' CppVector, CppDeque, CppForwardList, or CppList object of the same class and data type as \code{e1}.
#' 
#' @returns Returns \code{TRUE}, if the containers hold the same data and \code{FALSE} otherwise.
#' 
#' @seealso \link{contains}, \link{type}, \link{sorting}.
#' 
#' @examples
#' x <- cpp_set(1:10)
#' y <- cpp_set(1:10)
#' x == y
#' # [1] TRUE
#' 
#' y <- cpp_set(1:11)
#' x == y
#' # [1] FALSE
#' 
#' @include classes.R

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppSet", e2 = "CppSet"), function(e1, e2) {
  return(switch(e1@type,
    integer = set_equal_i(e1@pointer, e2@pointer),
    double = set_equal_d(e1@pointer, e2@pointer),
    string = set_equal_s(e1@pointer, e2@pointer),
    boolean = set_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppUnorderedSet", e2 = "CppUnorderedSet"), function(e1, e2) {
  return(switch(e1@type,
    integer = unordered_set_equal_i(e1@pointer, e2@pointer),
    double = unordered_set_equal_d(e1@pointer, e2@pointer),
    string = unordered_set_equal_s(e1@pointer, e2@pointer),
    boolean = unordered_set_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppMultiset", e2 = "CppMultiset"), function(e1, e2) {
  return(switch(e1@type,
    integer = multiset_equal_i(e1@pointer, e2@pointer),
    double = multiset_equal_d(e1@pointer, e2@pointer),
    string = multiset_equal_s(e1@pointer, e2@pointer),
    boolean = multiset_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppUnorderedMultiset", e2 = "CppUnorderedMultiset"), function(e1, e2) {
  return(switch(e1@type,
    integer = unordered_multiset_equal_i(e1@pointer, e2@pointer),
    double = unordered_multiset_equal_d(e1@pointer, e2@pointer),
    string = unordered_multiset_equal_s(e1@pointer, e2@pointer),
    boolean = unordered_multiset_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppMap", e2 = "CppMap"), function(e1, e2) {
  return(switch(e1@key_type,
    integer = switch(e1@value_type,
      integer = map_equal_i_i(e1@pointer, e2@pointer),
      double = map_equal_i_d(e1@pointer, e2@pointer),
      string = map_equal_i_s(e1@pointer, e2@pointer),
      boolean = map_equal_i_b(e1@pointer, e2@pointer)),
    double = switch(e1@value_type,
      integer = map_equal_d_i(e1@pointer, e2@pointer),
      double = map_equal_d_d(e1@pointer, e2@pointer),
      string = map_equal_d_s(e1@pointer, e2@pointer),
      boolean = map_equal_d_b(e1@pointer, e2@pointer)),
    string = switch(e1@value_type,
      integer = map_equal_s_i(e1@pointer, e2@pointer),
      double = map_equal_s_d(e1@pointer, e2@pointer),
      string = map_equal_s_s(e1@pointer, e2@pointer),
      boolean = map_equal_s_b(e1@pointer, e2@pointer)),
    boolean = switch(e1@value_type,
      integer = map_equal_b_i(e1@pointer, e2@pointer),
      double = map_equal_b_d(e1@pointer, e2@pointer),
      string = map_equal_b_s(e1@pointer, e2@pointer),
      boolean = map_equal_b_b(e1@pointer, e2@pointer))
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppUnorderedMap", e2 = "CppUnorderedMap"), function(e1, e2) {
  return(switch(e1@key_type,
    integer = switch(e1@value_type,
      integer = unordered_map_equal_i_i(e1@pointer, e2@pointer),
      double = unordered_map_equal_i_d(e1@pointer, e2@pointer),
      string = unordered_map_equal_i_s(e1@pointer, e2@pointer),
      boolean = unordered_map_equal_i_b(e1@pointer, e2@pointer)),
    double = switch(e1@value_type,
      integer = unordered_map_equal_d_i(e1@pointer, e2@pointer),
      double = unordered_map_equal_d_d(e1@pointer, e2@pointer),
      string = unordered_map_equal_d_s(e1@pointer, e2@pointer),
      boolean = unordered_map_equal_d_b(e1@pointer, e2@pointer)),
    string = switch(e1@value_type,
      integer = unordered_map_equal_s_i(e1@pointer, e2@pointer),
      double = unordered_map_equal_s_d(e1@pointer, e2@pointer),
      string = unordered_map_equal_s_s(e1@pointer, e2@pointer),
      boolean = unordered_map_equal_s_b(e1@pointer, e2@pointer)),
    boolean = switch(e1@value_type,
      integer = unordered_map_equal_b_i(e1@pointer, e2@pointer),
      double = unordered_map_equal_b_d(e1@pointer, e2@pointer),
      string = unordered_map_equal_b_s(e1@pointer, e2@pointer),
      boolean = unordered_map_equal_b_b(e1@pointer, e2@pointer))
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppMultimap", e2 = "CppMultimap"), function(e1, e2) {
  return(switch(e1@key_type,
    integer = switch(e1@value_type,
      integer = multimap_equal_i_i(e1@pointer, e2@pointer),
      double = multimap_equal_i_d(e1@pointer, e2@pointer),
      string = multimap_equal_i_s(e1@pointer, e2@pointer),
      boolean = multimap_equal_i_b(e1@pointer, e2@pointer)),
    double = switch(e1@value_type,
      integer = multimap_equal_d_i(e1@pointer, e2@pointer),
      double = multimap_equal_d_d(e1@pointer, e2@pointer),
      string = multimap_equal_d_s(e1@pointer, e2@pointer),
      boolean = multimap_equal_d_b(e1@pointer, e2@pointer)),
    string = switch(e1@value_type,
      integer = multimap_equal_s_i(e1@pointer, e2@pointer),
      double = multimap_equal_s_d(e1@pointer, e2@pointer),
      string = multimap_equal_s_s(e1@pointer, e2@pointer),
      boolean = multimap_equal_s_b(e1@pointer, e2@pointer)),
    boolean = switch(e1@value_type,
      integer = multimap_equal_b_i(e1@pointer, e2@pointer),
      double = multimap_equal_b_d(e1@pointer, e2@pointer),
      string = multimap_equal_b_s(e1@pointer, e2@pointer),
      boolean = multimap_equal_b_b(e1@pointer, e2@pointer))
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppUnorderedMultimap", e2 = "CppUnorderedMultimap"), function(e1, e2) {
  return(switch(e1@key_type,
    integer = switch(e1@value_type,
      integer = unordered_multimap_equal_i_i(e1@pointer, e2@pointer),
      double = unordered_multimap_equal_i_d(e1@pointer, e2@pointer),
      string = unordered_multimap_equal_i_s(e1@pointer, e2@pointer),
      boolean = unordered_multimap_equal_i_b(e1@pointer, e2@pointer)),
    double = switch(e1@value_type,
      integer = unordered_multimap_equal_d_i(e1@pointer, e2@pointer),
      double = unordered_multimap_equal_d_d(e1@pointer, e2@pointer),
      string = unordered_multimap_equal_d_s(e1@pointer, e2@pointer),
      boolean = unordered_multimap_equal_d_b(e1@pointer, e2@pointer)),
    string = switch(e1@value_type,
      integer = unordered_multimap_equal_s_i(e1@pointer, e2@pointer),
      double = unordered_multimap_equal_s_d(e1@pointer, e2@pointer),
      string = unordered_multimap_equal_s_s(e1@pointer, e2@pointer),
      boolean = unordered_multimap_equal_s_b(e1@pointer, e2@pointer)),
    boolean = switch(e1@value_type,
      integer = unordered_multimap_equal_b_i(e1@pointer, e2@pointer),
      double = unordered_multimap_equal_b_d(e1@pointer, e2@pointer),
      string = unordered_multimap_equal_b_s(e1@pointer, e2@pointer),
      boolean = unordered_multimap_equal_b_b(e1@pointer, e2@pointer))
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppStack", e2 = "CppStack"), function(e1, e2) {
  return(switch(e1@type,
    integer = stack_equal_i(e1@pointer, e2@pointer),
    double = stack_equal_d(e1@pointer, e2@pointer),
    string = stack_equal_s(e1@pointer, e2@pointer),
    boolean = stack_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppQueue", e2 = "CppQueue"), function(e1, e2) {
  return(switch(e1@type,
    integer = queue_equal_i(e1@pointer, e2@pointer),
    double = queue_equal_d(e1@pointer, e2@pointer),
    string = queue_equal_s(e1@pointer, e2@pointer),
    boolean = queue_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppVector", e2 = "CppVector"), function(e1, e2) {
  return(switch(e1@type,
    integer = vector_equal_i(e1@pointer, e2@pointer),
    double = vector_equal_d(e1@pointer, e2@pointer),
    string = vector_equal_s(e1@pointer, e2@pointer),
    boolean = vector_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppDeque", e2 = "CppDeque"), function(e1, e2) {
  return(switch(e1@type,
    integer = deque_equal_i(e1@pointer, e2@pointer),
    double = deque_equal_d(e1@pointer, e2@pointer),
    string = deque_equal_s(e1@pointer, e2@pointer),
    boolean = deque_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppForwardList", e2 = "CppForwardList"), function(e1, e2) {
  return(switch(e1@type,
    integer = forward_list_equal_i(e1@pointer, e2@pointer),
    double = forward_list_equal_d(e1@pointer, e2@pointer),
    string = forward_list_equal_s(e1@pointer, e2@pointer),
    boolean = forward_list_equal_b(e1@pointer, e2@pointer)
  ))
})

#' @rdname equal
#' @export
methods::setMethod("==", methods::signature(e1 = "CppList", e2 = "CppList"), function(e1, e2) {
  return(switch(e1@type,
    integer = list_equal_i(e1@pointer, e2@pointer),
    double = list_equal_d(e1@pointer, e2@pointer),
    string = list_equal_s(e1@pointer, e2@pointer),
    boolean = list_equal_b(e1@pointer, e2@pointer)
  ))
})

#' Access or insert elements without bounds checking
#' 
#' Read or insert a value by key in a CppMap or CppUnorderedMap. Read a value by index in a CppVector or CppDeque.
#' 
#' @param x A CppMap, CppUnorderedMap, CppVector, or CppDeque object.
#' @param i A key (CppMap, CppUnorderedMap) or index (CppVector, CppDeque).
#' 
#' @details In the two associative container types (CppMap, CppUnorderedMap), \code{[]} accesses a value by its key. If the key does not exist, it enters 
#' the key with a default value into the container. The default value is 0 for integer and double, an empty string for string, and \code{FALSE} for 
#' boolean.
#' 
#' In the two sequence container types (CppVector, CppDeque), \code{[]} accesses a value by its index. If the index is outside the container, this crashes 
#' the program.
#' 
#' \link{at} and \code{[]} both access elements. Unlike \code{[]}, \link{at} checks the bounds of the container and throws an error, if the element does 
#' not exist.
#' 
#' @returns Returns the value associated with \code{i}.
#' 
#' @seealso \link{at}, \link{back}, \link{contains}, \link{front}, \link{top}.
#' 
#' @examples
#' m <- cpp_map(4:6, seq.int(0, 1, by = 0.5))
#' m
#' # [4,0] [5,0.5] [6,1]
#' 
#' m[6L]
#' # [1] 1
#' 
#' m
#' # [4,0] [5,0.5] [6,1] 
#' 
#' m[8L]
#' # [1] 0
#' 
#' m
#' # [4,0] [5,0.5] [6,1] [8,0]
#' 
#' v <- cpp_vector(4:6)
#' v
#' # 4 5 6
#' 
#' v[1L]
#' # [1] 4
#' 
#' v
#' # 4 5 6
#' 

#' @rdname sub
#' @export
methods::setMethod("[", methods::signature(x = "CppMap"), function(x, i) {
  check_insert_value(i)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = map_bracket_i_i(x@pointer, i),
      double = map_bracket_i_d(x@pointer, i),
      string = map_bracket_i_s(x@pointer, i),
      boolean = map_bracket_i_b(x@pointer, i)),
    double = switch(x@value_type,
      integer = map_bracket_d_i(x@pointer, i),
      double = map_bracket_d_d(x@pointer, i),
      string = map_bracket_d_s(x@pointer, i),
      boolean = map_bracket_d_b(x@pointer, i)),
    string = switch(x@value_type,
      integer = map_bracket_s_i(x@pointer, i),
      double = map_bracket_s_d(x@pointer, i),
      string = map_bracket_s_s(x@pointer, i),
      boolean = map_bracket_s_b(x@pointer, i)),
    boolean = switch(x@value_type,
      integer = map_bracket_b_i(x@pointer, i),
      double = map_bracket_b_d(x@pointer, i),
      string = map_bracket_b_s(x@pointer, i),
      boolean = map_bracket_b_b(x@pointer, i))
  ))
})

#' @rdname sub
#' @export
methods::setMethod("[", methods::signature(x = "CppUnorderedMap"), function(x, i) {
  check_insert_value(i)
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_bracket_i_i(x@pointer, i),
      double = unordered_map_bracket_i_d(x@pointer, i),
      string = unordered_map_bracket_i_s(x@pointer, i),
      boolean = unordered_map_bracket_i_b(x@pointer, i)),
    double = switch(x@value_type,
      integer = unordered_map_bracket_d_i(x@pointer, i),
      double = unordered_map_bracket_d_d(x@pointer, i),
      string = unordered_map_bracket_d_s(x@pointer, i),
      boolean = unordered_map_bracket_d_b(x@pointer, i)),
    string = switch(x@value_type,
      integer = unordered_map_bracket_s_i(x@pointer, i),
      double = unordered_map_bracket_s_d(x@pointer, i),
      string = unordered_map_bracket_s_s(x@pointer, i),
      boolean = unordered_map_bracket_s_b(x@pointer, i)),
    boolean = switch(x@value_type,
      integer = unordered_map_bracket_b_i(x@pointer, i),
      double = unordered_map_bracket_b_d(x@pointer, i),
      string = unordered_map_bracket_b_s(x@pointer, i),
      boolean = unordered_map_bracket_b_b(x@pointer, i))
  ))
})

#' @rdname sub
#' @export
methods::setMethod("[", methods::signature(x = "CppVector"), function(x, i) {
  i <- i - 1L
  return(switch(x@type,
    integer = vector_bracket_i(x@pointer, i),
    double = vector_bracket_d(x@pointer, i),
    string = vector_bracket_s(x@pointer, i),
    boolean = vector_bracket_b(x@pointer, i)
  ))
})

#' @rdname sub
#' @export
methods::setMethod("[", methods::signature(x = "CppDeque"), function(x, i) {
  i <- i - 1L
  return(switch(x@type,
    integer = deque_bracket_i(x@pointer, i),
    double = deque_bracket_d(x@pointer, i),
    string = deque_bracket_s(x@pointer, i),
    boolean = deque_bracket_b(x@pointer, i)
  ))
})
