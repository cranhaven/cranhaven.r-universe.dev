can_candidate_be_removed <- function(obj,
                                     obj_arg_name,
                                     idx, 
                                     results_init, 
                                     fun,
                                     dont_touch,
                                     tolerate_warnings,
                                     ...) {
  
  # check if 'idx' matches something that must not be removed.
  # Of course only if its actually relevant..
  if (length(dont_touch) > 0) {
    # convert candidate idx to position named index.
    idx_name <- tryCatch(convert_idx_to_name(idx, obj),
                         error = function(e) {NULL})
    if (!is.null(idx_name)) {
      # does idx match any of elements in 'dont_touch'?
      violations <- vapply(dont_touch, match_entries, FUN.VALUE = logical(1L), 
                           match_against = idx_name)
      # return FALSE early, if there was any matches.
      if (any(violations)) {
        return(FALSE)
      }
    }
  }

  # remove entry in list.
  obj[[idx]] <- NULL
  
  # compute results with object after removal.
  results <- get_results_for_object(obj, obj_arg_name, fun, 
                                    tolerate_warnings = tolerate_warnings, ...) 
  
  # were errors encountered?
  if (inherits(results, "error")) {
    return(FALSE)
  }
  
  # were warnings encountered?
  if (!tolerate_warnings && inherits(results, "warning")) {
    return(FALSE)
  }
  
  # check if results are identical.
  identical(results_init, results)
  
}
