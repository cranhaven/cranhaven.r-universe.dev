#' Get the mean number of elements per bucket
#' 
#' Get the mean number of elements per bucket.
#' 
#' @param x A CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, or CppUnorderedMultimap object.
#' 
#' @returns Returns a numeric.
#' 
#' @seealso \link{bucket_count}, \link{max_bucket_count}, \link{max_load_factor}.
#' 
#' @examples
#' s <- cpp_unordered_set(6:9)
#' load_factor(s)
#' # [1] 0.3076923
#' 

#' @aliases load_factor,CppUnorderedSet-method load_factor,CppUnorderedMultiset-method load_factor,CppUnorderedMap-method 
#' load_factor,CppUnorderedMultimap-method

#' @export
methods::setGeneric("load_factor", function(x) standardGeneric("load_factor"))

#' @include classes.R

#' @export
methods::setMethod("load_factor", methods::signature(x = "CppUnorderedSet"), function(x) {
  return(switch(x@type,
    integer = unordered_set_load_factor_i(x@pointer),
    double = unordered_set_load_factor_d(x@pointer),
    string = unordered_set_load_factor_s(x@pointer),
    boolean = unordered_set_load_factor_b(x@pointer)
  ))
})

#' @export
methods::setMethod("load_factor", methods::signature(x = "CppUnorderedMultiset"), function(x) {
  return(switch(x@type,
    integer = unordered_multiset_load_factor_i(x@pointer),
    double = unordered_multiset_load_factor_d(x@pointer),
    string = unordered_multiset_load_factor_s(x@pointer),
    boolean = unordered_multiset_load_factor_b(x@pointer)
  ))
})

#' @export
methods::setMethod("load_factor", methods::signature(x = "CppUnorderedMap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_map_load_factor_i_i(x@pointer),
      double = unordered_map_load_factor_i_d(x@pointer),
      string = unordered_map_load_factor_i_s(x@pointer),
      boolean = unordered_map_load_factor_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_map_load_factor_d_i(x@pointer),
      double = unordered_map_load_factor_d_d(x@pointer),
      string = unordered_map_load_factor_d_s(x@pointer),
      boolean = unordered_map_load_factor_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_map_load_factor_s_i(x@pointer),
      double = unordered_map_load_factor_s_d(x@pointer),
      string = unordered_map_load_factor_s_s(x@pointer),
      boolean = unordered_map_load_factor_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_map_load_factor_b_i(x@pointer),
      double = unordered_map_load_factor_b_d(x@pointer),
      string = unordered_map_load_factor_b_s(x@pointer),
      boolean = unordered_map_load_factor_b_b(x@pointer))
  ))
})

#' @export
methods::setMethod("load_factor", methods::signature(x = "CppUnorderedMultimap"), function(x) {
  return(switch(x@key_type,
    integer = switch(x@value_type,
      integer = unordered_multimap_load_factor_i_i(x@pointer),
      double = unordered_multimap_load_factor_i_d(x@pointer),
      string = unordered_multimap_load_factor_i_s(x@pointer),
      boolean = unordered_multimap_load_factor_i_b(x@pointer)),
    double = switch(x@value_type,
      integer = unordered_multimap_load_factor_d_i(x@pointer),
      double = unordered_multimap_load_factor_d_d(x@pointer),
      string = unordered_multimap_load_factor_d_s(x@pointer),
      boolean = unordered_multimap_load_factor_d_b(x@pointer)),
    string = switch(x@value_type,
      integer = unordered_multimap_load_factor_s_i(x@pointer),
      double = unordered_multimap_load_factor_s_d(x@pointer),
      string = unordered_multimap_load_factor_s_s(x@pointer),
      boolean = unordered_multimap_load_factor_s_b(x@pointer)),
    boolean = switch(x@value_type,
      integer = unordered_multimap_load_factor_b_i(x@pointer),
      double = unordered_multimap_load_factor_b_d(x@pointer),
      string = unordered_multimap_load_factor_b_s(x@pointer),
      boolean = unordered_multimap_load_factor_b_b(x@pointer))
  ))
})
