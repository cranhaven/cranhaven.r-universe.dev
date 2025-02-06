#' Get the maximum number of buckets
#' 
#' Obtain the maximum number of buckets the container can hold.
#' 
#' @param x A CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, or CppUnorderedMultimap object.
#' 
#' @returns Returns a numeric.
#' 
#' @seealso \link{bucket_count}, \link{load_factor}, \link{max_load_factor}.
#' 
#' @examples
#' s <- cpp_unordered_set(6:10)
#' max_bucket_count(s)
#' # [1] 1.152922e+18
#' 

#' @aliases max_bucket_count,CppUnorderedSet-method max_bucket_count,CppUnorderedMultiset-method max_bucket_count,CppUnorderedMap-method 
#' max_bucket_count,CppUnorderedMultimap-method

#' @export
methods::setGeneric("max_bucket_count", function(x) standardGeneric("max_bucket_count"))

#' @include classes.R

#' @export
methods::setMethod("max_bucket_count", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_max_bucket_count_i(x@pointer),
    double = unordered_set_max_bucket_count_d(x@pointer),
    string = unordered_set_max_bucket_count_s(x@pointer),
    boolean = unordered_set_max_bucket_count_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_bucket_count", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_max_bucket_count_i(x@pointer),
    double = unordered_multiset_max_bucket_count_d(x@pointer),
    string = unordered_multiset_max_bucket_count_s(x@pointer),
    boolean = unordered_multiset_max_bucket_count_b(x@pointer)
  ))
})

#' @export
methods::setMethod("max_bucket_count", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_max_bucket_count_i_i(x@pointer),
      double = unordered_map_max_bucket_count_i_d(x@pointer),
      string = unordered_map_max_bucket_count_i_s(x@pointer),
      boolean = unordered_map_max_bucket_count_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_max_bucket_count_d_i(x@pointer),
      double = unordered_map_max_bucket_count_d_d(x@pointer),
      string = unordered_map_max_bucket_count_d_s(x@pointer),
      boolean = unordered_map_max_bucket_count_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_max_bucket_count_s_i(x@pointer),
      double = unordered_map_max_bucket_count_s_d(x@pointer),
      string = unordered_map_max_bucket_count_s_s(x@pointer),
      boolean = unordered_map_max_bucket_count_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_max_bucket_count_b_i(x@pointer),
      double = unordered_map_max_bucket_count_b_d(x@pointer),
      string = unordered_map_max_bucket_count_b_s(x@pointer),
      boolean = unordered_map_max_bucket_count_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("max_bucket_count", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_max_bucket_count_i_i(x@pointer),
      double = unordered_multimap_max_bucket_count_i_d(x@pointer),
      string = unordered_multimap_max_bucket_count_i_s(x@pointer),
      boolean = unordered_multimap_max_bucket_count_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_max_bucket_count_d_i(x@pointer),
      double = unordered_multimap_max_bucket_count_d_d(x@pointer),
      string = unordered_multimap_max_bucket_count_d_s(x@pointer),
      boolean = unordered_multimap_max_bucket_count_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_max_bucket_count_s_i(x@pointer),
      double = unordered_multimap_max_bucket_count_s_d(x@pointer),
      string = unordered_multimap_max_bucket_count_s_s(x@pointer),
      boolean = unordered_multimap_max_bucket_count_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_max_bucket_count_b_i(x@pointer),
      double = unordered_multimap_max_bucket_count_b_d(x@pointer),
      string = unordered_multimap_max_bucket_count_b_s(x@pointer),
      boolean = unordered_multimap_max_bucket_count_b_b(x@pointer))
  ))
})
