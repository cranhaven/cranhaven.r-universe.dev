#' Choose columns by name, by prefix or by type
#'
#' `y`, `x` and `static` in [timesift()] are tidyselect expressions, so a response spread over a
#' hundred columns is named once, as `y = starts_with("sp_")`, and the value columns of a series
#' carrying several sensors are named the same way. These are tidyselect's own helpers,
#' re-exported so that attaching timesift is enough to reach them and a session attaching both
#' packages still has one implementation of each.
#'
#' @name select_helpers
#' @keywords internal
NULL

#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with

#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with

#' @importFrom tidyselect contains
#' @export
tidyselect::contains

#' @importFrom tidyselect matches
#' @export
tidyselect::matches

#' @importFrom tidyselect all_of
#' @export
tidyselect::all_of

#' @importFrom tidyselect any_of
#' @export
tidyselect::any_of

#' @importFrom tidyselect everything
#' @export
tidyselect::everything

#' @importFrom tidyselect where
#' @export
tidyselect::where

# A tidyselect expression reaches the fitting layer as the column names it chose. An argument left
# at NULL, or not given at all, chooses nothing; what a caller who chose nothing gets instead is
# the entry point's business and differs between `x` and `static`.
.select_columns <- function(quo, data, arg) {
  if (is.null(data) || rlang::quo_is_missing(quo) || rlang::quo_is_null(quo)) {
    return(character())
  }
  pos <- tidyselect::eval_select(quo, data)
  unname(names(data)[pos])
}

# `id`, `time` and `target_time` are single columns rather than selections, and are resolved by
# the same rule `grain_matrix()` resolves its own three by. NULL is how an optional one is left out.
.optional_column <- function(expr, data, envir) {
  if (is.null(expr)) {
    return(NULL)
  }
  .resolve_column(expr, data, envir)
}

# Names in a message, with a tail the reader does not need spelled out.
.listing <- function(x, n = 3L) {
  x <- as.character(x)
  if (length(x) <= n) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[seq_len(n)], collapse = ", "), " and ", length(x) - n, " more")
}
