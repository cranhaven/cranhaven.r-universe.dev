#' Get or set the maximum load factor
#' 
#' Get or set the maximum load factor by reference, i.e. the number of elements per bucket.
#' 
#' @param x A CppUnorderedSet, CppUnorderedMultiset, CppUnorderedMap, or CppUnorderedMultimap object.
#' @param max_load The containers maximum load factor. If \code{NULL}, the function returns the container's current maximum load factor. Passing a number 
#' sets the maximum load factor to that value.
#' 
#' @returns Returns a numeric, if \code{max_load} is \code{NULL}. Invisibly returns \code{NULL}, if \code{max_load} is numeric.
#' 
#' @seealso \link{bucket_count}, \link{load_factor}, \link{max_bucket_count}.
#' 
#' @examples
#' s <- cpp_unordered_set(4:6)
#' max_load_factor(s)
#' # [1] 1
#' 
#' max_load_factor(s, 3)
#' max_load_factor(s)
#' # [1] 3
#' 

#' @aliases max_load_factor,CppUnorderedSet-method max_load_factor,CppUnorderedMultiset-method max_load_factor,CppUnorderedMap-method 
#' max_load_factor,CppUnorderedMultimap-method

#' @export
methods::setGeneric("max_load_factor", function(x, max_load = NULL) standardGeneric("max_load_factor"))

#' @include classes.R

#' @export
methods::setMethod("max_load_factor", methods::signature(x = "CppUnorderedSet"), function(x, max_load = NULL) {
  if(is.null(max_load)) {
    return(switch(x@type,
      integer = unordered_set_max_load_factor_i_get(x@pointer),
      double = unordered_set_max_load_factor_d_get(x@pointer),
      string = unordered_set_max_load_factor_s_get(x@pointer),
      boolean = unordered_set_max_load_factor_b_get(x@pointer)
    ))
  } else {
    if(!is.numeric(max_load) || !is.finite(max_load[1L])) {
      stop("max_load must be either NULL or a positive finite number.")
    }
    return(switch(x@type,
      integer = unordered_set_max_load_factor_i_set(x@pointer, max_load),
      double = unordered_set_max_load_factor_d_set(x@pointer, max_load),
      string = unordered_set_max_load_factor_s_set(x@pointer, max_load),
      boolean = unordered_set_max_load_factor_b_set(x@pointer, max_load)
    ))
  }
})

#' @export
methods::setMethod("max_load_factor", methods::signature(x = "CppUnorderedMultiset"), function(x, max_load = NULL) {
  if(is.null(max_load)) {
    return(switch(x@type,
      integer = unordered_multiset_max_load_factor_i_get(x@pointer),
      double = unordered_multiset_max_load_factor_d_get(x@pointer),
      string = unordered_multiset_max_load_factor_s_get(x@pointer),
      boolean = unordered_multiset_max_load_factor_b_get(x@pointer)
    ))
  } else {
    if(!is.numeric(max_load) || !is.finite(max_load[1L])) {
      stop("max_load must be either NULL or a positive finite number.")
    }
    return(switch(x@type,
      integer = unordered_multiset_max_load_factor_i_set(x@pointer, max_load),
      double = unordered_multiset_max_load_factor_d_set(x@pointer, max_load),
      string = unordered_multiset_max_load_factor_s_set(x@pointer, max_load),
      boolean = unordered_multiset_max_load_factor_b_set(x@pointer, max_load)
    ))
  }
})

#' @export
methods::setMethod("max_load_factor", methods::signature(x = "CppUnorderedMap"), function(x, max_load = NULL) {
  if(is.null(max_load)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_map_max_load_factor_i_i_get(x@pointer),
        double = unordered_map_max_load_factor_i_d_get(x@pointer),
        string = unordered_map_max_load_factor_i_s_get(x@pointer),
        boolean = unordered_map_max_load_factor_i_b_get(x@pointer)),
      double = switch(x@value_type,
        integer = unordered_map_max_load_factor_d_i_get(x@pointer),
        double = unordered_map_max_load_factor_d_d_get(x@pointer),
        string = unordered_map_max_load_factor_d_s_get(x@pointer),
        boolean = unordered_map_max_load_factor_d_b_get(x@pointer)),
      string = switch(x@value_type,
        integer = unordered_map_max_load_factor_s_i_get(x@pointer),
        double = unordered_map_max_load_factor_s_d_get(x@pointer),
        string = unordered_map_max_load_factor_s_s_get(x@pointer),
        boolean = unordered_map_max_load_factor_s_b_get(x@pointer)),
      boolean = switch(x@value_type,
        integer = unordered_map_max_load_factor_b_i_get(x@pointer),
        double = unordered_map_max_load_factor_b_d_get(x@pointer),
        string = unordered_map_max_load_factor_b_s_get(x@pointer),
        boolean = unordered_map_max_load_factor_b_b_get(x@pointer))
    ))
  } else {
    if(!is.numeric(max_load) || !is.finite(max_load[1L])) {
      stop("max_load must be either NULL or a positive finite number.")
    }
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_map_max_load_factor_i_i_set(x@pointer, max_load),
        double = unordered_map_max_load_factor_i_d_set(x@pointer, max_load),
        string = unordered_map_max_load_factor_i_s_set(x@pointer, max_load),
        boolean = unordered_map_max_load_factor_i_b_set(x@pointer, max_load)),
      double = switch(x@value_type,
        integer = unordered_map_max_load_factor_d_i_set(x@pointer, max_load),
        double = unordered_map_max_load_factor_d_d_set(x@pointer, max_load),
        string = unordered_map_max_load_factor_d_s_set(x@pointer, max_load),
        boolean = unordered_map_max_load_factor_d_b_set(x@pointer, max_load)),
      string = switch(x@value_type,
        integer = unordered_map_max_load_factor_s_i_set(x@pointer, max_load),
        double = unordered_map_max_load_factor_s_d_set(x@pointer, max_load),
        string = unordered_map_max_load_factor_s_s_set(x@pointer, max_load),
        boolean = unordered_map_max_load_factor_s_b_set(x@pointer, max_load)),
      boolean = switch(x@value_type,
        integer = unordered_map_max_load_factor_b_i_set(x@pointer, max_load),
        double = unordered_map_max_load_factor_b_d_set(x@pointer, max_load),
        string = unordered_map_max_load_factor_b_s_set(x@pointer, max_load),
        boolean = unordered_map_max_load_factor_b_b_set(x@pointer, max_load))
    ))
  }
})

#' @export
methods::setMethod("max_load_factor", methods::signature(x = "CppUnorderedMultimap"), function(x, max_load = NULL) {
  if(is.null(max_load)) {
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_i_i_get(x@pointer),
        double = unordered_multimap_max_load_factor_i_d_get(x@pointer),
        string = unordered_multimap_max_load_factor_i_s_get(x@pointer),
        boolean = unordered_multimap_max_load_factor_i_b_get(x@pointer)),
      double = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_d_i_get(x@pointer),
        double = unordered_multimap_max_load_factor_d_d_get(x@pointer),
        string = unordered_multimap_max_load_factor_d_s_get(x@pointer),
        boolean = unordered_multimap_max_load_factor_d_b_get(x@pointer)),
      string = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_s_i_get(x@pointer),
        double = unordered_multimap_max_load_factor_s_d_get(x@pointer),
        string = unordered_multimap_max_load_factor_s_s_get(x@pointer),
        boolean = unordered_multimap_max_load_factor_s_b_get(x@pointer)),
      boolean = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_b_i_get(x@pointer),
        double = unordered_multimap_max_load_factor_b_d_get(x@pointer),
        string = unordered_multimap_max_load_factor_b_s_get(x@pointer),
        boolean = unordered_multimap_max_load_factor_b_b_get(x@pointer))
    ))
  } else {
    if(!is.numeric(max_load) || !is.finite(max_load[1L])) {
      stop("max_load must be either NULL or a positive finite number.")
    }
    return(switch(x@key_type,
      integer = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_i_i_set(x@pointer, max_load),
        double = unordered_multimap_max_load_factor_i_d_set(x@pointer, max_load),
        string = unordered_multimap_max_load_factor_i_s_set(x@pointer, max_load),
        boolean = unordered_multimap_max_load_factor_i_b_set(x@pointer, max_load)),
      double = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_d_i_set(x@pointer, max_load),
        double = unordered_multimap_max_load_factor_d_d_set(x@pointer, max_load),
        string = unordered_multimap_max_load_factor_d_s_set(x@pointer, max_load),
        boolean = unordered_multimap_max_load_factor_d_b_set(x@pointer, max_load)),
      string = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_s_i_set(x@pointer, max_load),
        double = unordered_multimap_max_load_factor_s_d_set(x@pointer, max_load),
        string = unordered_multimap_max_load_factor_s_s_set(x@pointer, max_load),
        boolean = unordered_multimap_max_load_factor_s_b_set(x@pointer, max_load)),
      boolean = switch(x@value_type,
        integer = unordered_multimap_max_load_factor_b_i_set(x@pointer, max_load),
        double = unordered_multimap_max_load_factor_b_d_set(x@pointer, max_load),
        string = unordered_multimap_max_load_factor_b_s_set(x@pointer, max_load),
        boolean = unordered_multimap_max_load_factor_b_b_set(x@pointer, max_load))
    ))
  }
})
