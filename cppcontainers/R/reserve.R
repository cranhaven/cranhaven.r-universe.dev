#' Reserve space
#' 
#' Reserve space for the container by reference.
#' 
#' @param x A CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, CppUnorderedMultimap, or CppVector object.
#' @param n The minimum number of elements per bucket.
#' 
#' @details In case of a CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, CppUnorderedMultimap, the method sets the number of buckets to be able to 
#' hold at least \code{n} elements and rehashes. In case of a CppVector, the method sets the capacity to \code{n}.
#' 
#' @returns Invisibly returns \code{NULL}.
#' 
#' @seealso \link{bucket_count}, \link{capacity}, \link{load_factor}, \link{max_bucket_count}, \link{max_load_factor}.
#' 
#' @examples
#' s <- cpp_unordered_set(4:6)
#' bucket_count(s)
#' # [1] 13
#' reserve(s, 3)
#' bucket_count(s)
#' # [1] 5
#' 
#' v <- cpp_vector(4:6)
#' capacity(v)
#' # [1] 3
#' reserve(v, 10)
#' capacity(v)
#' # [1] 10
#' 

#' @aliases reserve,CppUnorderedSet-method reserve,CppUnorderedMultiset-method reserve,CppUnorderedMap-method reserve,CppUnorderedMultimap-method 
#' reserve,CppVector-method

#' @export
methods::setGeneric("reserve", function(x, n) standardGeneric("reserve"))

#' @include classes.R

#' @export
methods::setMethod("reserve", methods::signature(x = "CppUnorderedSet"), function(x, n) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = unordered_set_reserve_i(x@pointer, n),
    double = unordered_set_reserve_d(x@pointer, n),
    string = unordered_set_reserve_s(x@pointer, n),
    boolean = unordered_set_reserve_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("reserve", methods::signature(x = "CppUnorderedMultiset"), function(x, n) {
  if(!is.finite(n[1L])) {
    stop("n must be a weakly positive finite number.")
  }
  return(switch(x@type,
    integer = unordered_multiset_reserve_i(x@pointer, n),
    double = unordered_multiset_reserve_d(x@pointer, n),
    string = unordered_multiset_reserve_s(x@pointer, n),
    boolean = unordered_multiset_reserve_b(x@pointer, n)
  ))
})

#' @export
methods::setMethod("reserve", methods::signature(x = "CppUnorderedMap"), function(x, n) {
  if(length(n) != 1L || !is.numeric(n) || !is.finite(n) || n <= 0L) {
    stop("n must be a positive finite number.")
  }
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_reserve_i_i(x@pointer, n),
      double = unordered_map_reserve_i_d(x@pointer, n),
      string = unordered_map_reserve_i_s(x@pointer, n),
      boolean = unordered_map_reserve_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_map_reserve_d_i(x@pointer, n),
      double = unordered_map_reserve_d_d(x@pointer, n),
      string = unordered_map_reserve_d_s(x@pointer, n),
      boolean = unordered_map_reserve_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_map_reserve_s_i(x@pointer, n),
      double = unordered_map_reserve_s_d(x@pointer, n),
      string = unordered_map_reserve_s_s(x@pointer, n),
      boolean = unordered_map_reserve_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_map_reserve_b_i(x@pointer, n),
      double = unordered_map_reserve_b_d(x@pointer, n),
      string = unordered_map_reserve_b_s(x@pointer, n),
      boolean = unordered_map_reserve_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("reserve", methods::signature(x = "CppUnorderedMultimap"), function(x, n) {
  if(length(n) != 1L || !is.numeric(n) || !is.finite(n) || n <= 0L) {
    stop("n must be a positive finite number.")
  }
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_reserve_i_i(x@pointer, n),
      double = unordered_multimap_reserve_i_d(x@pointer, n),
      string = unordered_multimap_reserve_i_s(x@pointer, n),
      boolean = unordered_multimap_reserve_i_b(x@pointer, n)),
    double = switch(x@value_type,
      integer = unordered_multimap_reserve_d_i(x@pointer, n),
      double = unordered_multimap_reserve_d_d(x@pointer, n),
      string = unordered_multimap_reserve_d_s(x@pointer, n),
      boolean = unordered_multimap_reserve_d_b(x@pointer, n)),
    string = switch(x@value_type,
      integer = unordered_multimap_reserve_s_i(x@pointer, n),
      double = unordered_multimap_reserve_s_d(x@pointer, n),
      string = unordered_multimap_reserve_s_s(x@pointer, n),
      boolean = unordered_multimap_reserve_s_b(x@pointer, n)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_reserve_b_i(x@pointer, n),
      double = unordered_multimap_reserve_b_d(x@pointer, n),
      string = unordered_multimap_reserve_b_s(x@pointer, n),
      boolean = unordered_multimap_reserve_b_b(x@pointer, n))
  ))
})

#' @export
methods::setMethod("reserve", methods::signature(x = "CppVector"), function(x, n) {
  if(length(n) != 1L || !is.numeric(n) || !is.finite(n) || n <= 0L) {
    stop("n must be a positive finite number.")
  }
  return(switch(x@type,
    integer = vector_reserve_i(x@pointer, n),
    double = vector_reserve_d(x@pointer, n),
    string = vector_reserve_s(x@pointer, n),
    boolean = vector_reserve_b(x@pointer, n)
  ))
})
