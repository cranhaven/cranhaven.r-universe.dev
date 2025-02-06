#' Set minimum bucket count and rehash
#' 
#' Set a container's minimum bucket count and rehash by reference.
#' 
#' @param x A CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, or CppUnorderedMultimap object.
#' @param n The minimum number of buckets. A value of \code{0} forces an unconditional rehash.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{bucket_count}, \link{load_factor}, \link{max_bucket_count}, \link{max_load_factor}, \link{reserve}.
#' 
#' @examples
#' s <- cpp_unordered_set(4:6)
#' rehash(s)
#' rehash(s, 3)
#' 

#' @aliases rehash,CppUnorderedSet-method rehash,CppUnorderedMultiset-method rehash,CppUnorderedMap-method rehash,CppUnorderedMultimap-method

#' @export
methods::setGeneric("rehash", function(x, n = 0) standardGeneric("rehash"))

#' @include classes.R

#' @export
methods::setMethod("rehash", methods::signature(x = "CppUnorderedSet"), function(x, n = 0) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = unordered_set_rehash_i(x@pointer, n),
    double = unordered_set_rehash_d(x@pointer, n),
    string = unordered_set_rehash_s(x@pointer, n),
    boolean = unordered_set_rehash_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("rehash", methods::signature(x = "CppUnorderedMultiset"), function(x, n = 0) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = unordered_multiset_rehash_i(x@pointer, n),
    double = unordered_multiset_rehash_d(x@pointer, n),
    string = unordered_multiset_rehash_s(x@pointer, n),
    boolean = unordered_multiset_rehash_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("rehash", methods::signature(x = "CppUnorderedMap"), function(x, n = 0) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_rehash_i_i(x@pointer, n),
      double = unordered_map_rehash_i_d(x@pointer, n),
      string = unordered_map_rehash_i_s(x@pointer, n),
      boolean = unordered_map_rehash_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_map_rehash_d_i(x@pointer, n),
      double = unordered_map_rehash_d_d(x@pointer, n),
      string = unordered_map_rehash_d_s(x@pointer, n),
      boolean = unordered_map_rehash_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_map_rehash_s_i(x@pointer, n),
      double = unordered_map_rehash_s_d(x@pointer, n),
      string = unordered_map_rehash_s_s(x@pointer, n),
      boolean = unordered_map_rehash_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_map_rehash_b_i(x@pointer, n),
      double = unordered_map_rehash_b_d(x@pointer, n),
      string = unordered_map_rehash_b_s(x@pointer, n),
      boolean = unordered_map_rehash_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("rehash", methods::signature(x = "CppUnorderedMultimap"), function(x, n = 0) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_rehash_i_i(x@pointer, n),
      double = unordered_multimap_rehash_i_d(x@pointer, n),
      string = unordered_multimap_rehash_i_s(x@pointer, n),
      boolean = unordered_multimap_rehash_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_multimap_rehash_d_i(x@pointer, n),
      double = unordered_multimap_rehash_d_d(x@pointer, n),
      string = unordered_multimap_rehash_d_s(x@pointer, n),
      boolean = unordered_multimap_rehash_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_multimap_rehash_s_i(x@pointer, n),
      double = unordered_multimap_rehash_s_d(x@pointer, n),
      string = unordered_multimap_rehash_s_s(x@pointer, n),
      boolean = unordered_multimap_rehash_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_rehash_b_i(x@pointer, n),
      double = unordered_multimap_rehash_b_d(x@pointer, n),
      string = unordered_multimap_rehash_b_s(x@pointer, n),
      boolean = unordered_multimap_rehash_b_b(x@pointer, n))
  ))
})
