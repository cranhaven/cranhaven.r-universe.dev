order_after_size <- function(dt) {
  dt <- copy(dt)
  setorder(dt, -size)
  dt
}

#' Compute Results From Function Call with Object as Argument
#'
#' @inheritParams trim
#' 
#' @return results from function call.
get_results_for_object <- function(obj, 
                                   obj_arg_name, 
                                   fun, 
                                   ...,
                                   tolerate_warnings = TRUE) {
  args_list <- list(...)
  obj_list <- list(obj)
  # set name of object arg if provided.
  if (!is.null(obj_arg_name)) {
    names(obj_list) <- obj_arg_name
  }
  args_list <- append(obj_list, args_list)
  if (tolerate_warnings) {
    res <- tryCatch(do.call(fun, args_list),
                    error = function(e) {e})
  } else {
    res <- tryCatch(do.call(fun, args_list),
                    error = function(e) {e},
                    warning = function(w) {w})
  }
  res
}

check_initial_results <- function(results_init, tolerate_warnings) {
  
  # hard stop if function call failed.
  if (inherits(results_init, "error")) {
    stop("Reference function call failed. Check error message below. \n",
         results_init)
  }
  
  # hard stop if function call failed.
  if (!tolerate_warnings && inherits(results_init, "warning")) {
    stop("Reference function call resulted in warning, which is not allowed when ",
    "tolerate_warnings is set to FALSE. Check warning message below. \n",
    results_init)
  }
  
  # invisible return.
  invisible(NULL)
  
}

check_inputs <- function(obj, obj_arg_name, fun, dont_touch) {
  
  # check 'dont_touch'.
  if (!is.list(dont_touch)) {
    stop("This function only accepts objects (`dont_touch`), that inherit from the",
         "'list' class.")
  }
  
  if (length(dont_touch) > 0) {
    if (!all(sapply(dont_touch, is.character))) {
      stop("elements in 'dont_touch' must be 'character'.")
    }
  }
  
  if (!is.list(obj)) {
    stop("This function only accepts objects (`obj`), that inherit from the",
         "'list' class.")
  }
  
  if (!is.function(fun)) {
    stop("'fun' must be a function.")
  }
  
  if (!is.null(obj_arg_name)) {
    if (!is.character(obj_arg_name)) {
      stop("'obj_arg_name' must be of type 'character'.")
    }
  } else {
    warning("No 'obj_arg_name' provided. Assumes that object matches first argument of 'fun'.")
  }
  
  # return invisibly.
  invisible(NULL)
  
}

get_length_candidate <- function(x, idx) {
  
  # check if element is list.
  if (!is.list(x[[idx]])) {
    return(NULL)
  }
  
  # check if element is actuall a data.frame.
  if (is.list(x[[idx]]) && is.data.frame(x[[idx]])) {
    return(NULL)
  }
  
  length(x[[idx]])
  
} 


#' Convert Size in Bytes to Print Friendly String
#'
#' @param x \code{numeric} object size in digits.
#' @param digits \code{numeric} number of digits you want.
#'
#' @return \code{character} priend friendly string.
#'
#' @export
#'
#' @examples
#' pf_obj_size(10)
#' pf_obj_size(1010)
#' pf_obj_size(2e06)
pf_obj_size <- function(x, digits = 2) {
  
  # convert to MB if bigger than one MB.
  if (x >= 1e06) {
    x <- x * 1e-06
    unit <- "MB"
  } else if(x >= 1e03) {
    # convert to kB if bigger than one 1 kB.
    x <- x * 1e-03
    unit <- "kB"
  } else {
    unit <- "B"
  }
  
  # round.
  x <- round(x, digits = digits)
  
  # convert to priend friendly string.
  paste0(c(x, unit), collapse = " ")
  
}

match_entries <- function(idx, match_against) {
  match_length <- min(length(idx), length(match_against))
  identical(idx[seq_len(match_length)], match_against[seq_len(match_length)])
}
