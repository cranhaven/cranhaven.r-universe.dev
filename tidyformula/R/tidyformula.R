#' Build a formula using `tidyselect`-style selection helpers
#'
#' @description
#' `tidyformula()` translates formulas containing `tidyselect`-style
#' [selection helpers][tidyselect::language], expanding these helpers by evaluating
#' [`dplyr::select()`] with the relevant selection helper and a supplied data frame.
#'
#' When the selection helper appears as the first argument of a function, that
#' function is distributed across the sum of the selected variables.
#' 
#' @param formula An object of class [`formula`]. Can contain selection helpers
#' to be expanded.
#' @param data A data frame whose column names should be used for selection
#' @param select_helpers A character vector. The names of selection helpers to
#' be matched and substituted.
#' @param nodistribute A character vector. Functions with these names are not
#' distributed over selection helpers.
#' @param env The environment to associate with the result.
#'
#' @return An object of class [`formula`], which is a translation of the argument
#' `formula` in which the selection helpers are replaced with the corresponding
#' variables of `data`.
#'
#' @seealso [`dplyr::select()`], [`tidyselect::language`] for
#' documentation of selection helpers.
#' 
#' @section Examples:
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' df <- data.frame(
#'   x1 = rnorm(5),
#'   x2 = rnorm(5),
#'   x3 = rnorm(5),
#'   y  = rnorm(5)
#' )
#' 
#' tidyformula(y ~ num_range("x", 1:2) + z, data = df)
#'
#' tidyformula(y ~ poly(starts_with("x"), 3), data = df)
#'
#' tidyformula( ~ everything() * contains("x"), data = df)
#'```
#'
#' Interaction operators are typically not distributed, but this behaviour can be changed.
#' ```{r, comment = "#>", collapse = TRUE}
#' tidyformula(y ~ starts_with("x")^2, data = df)
#' 
#' tidyformula(y ~ starts_with("x")^2, data = df, nodistribute = c("+", "-"))
#' ```
#' @export
tidyformula <- function(formula,
                        data,
                        select_helpers = .select_helpers,
                        nodistribute   = c("+", "-", "*", "^"),
                        env            = rlang::caller_env()) {

  # To avoid passing a big data frame around in the internal functions, drop
  # all of the rows of data.
  #
  # R has strange behaviour, where if a base R data frame has a single column, then slicing
  # rows using data[v, ] returns a vector. So account for this.
  #
  # tibbles do not have this issue, so we check if the class of data is exactly equal to
  # data.frame rather than using is.data.frame()
  if(setequal(class(data), "data.frame") && ncol(data) == 1) {
    data_colnamesonly <- data.frame(data[NULL,])
    names(data_colnamesonly) <- names(data)
  } else {
    data_colnamesonly <- data[NULL, ]
  }
  
  eval(
    replace_expr(formula, data_colnamesonly, select_helpers, nodistribute),
    envir = env
  )
}


