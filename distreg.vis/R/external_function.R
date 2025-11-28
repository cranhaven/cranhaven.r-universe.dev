#' External Function Implementer
#'
#' This function exists to extend \code{plot_moments} such that an external
#' function, which is user-written, can be included. Thus, the user can see the
#' impact of a variable on a self-defined measure, like the Gini Index.
#' @keywords internal

ex_f <- function(pred_params, unquotedfun) {

  # Stop if not function
  if (!is(unquotedfun, "function"))
    stop("Argument 'ex-fun' has to be a function!")

  # Try out whether function works
  vals <- unquotedfun(pred_params)

  # Return values
  return(vals)
}

