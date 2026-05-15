#' Reorder factor levels within groups (internal)
#'
#' Reorders labels in `x` **within** each level of `within` using `by`.
#' Returns a factor whose levels are the concatenation `paste(x, within, sep)`,
#' ordered by `fun(by)` computed per label-within-group.
#'
#' @param x      Character or factor of labels.
#' @param by     Numeric (or orderable) vector of the same length as `x`.
#' @param within Grouping factor/character of the same length as `x`.
#' @param fun    Summary function applied to `by` within each `(x, within)` pair.
#'               Default: mean with `na.rm = TRUE`.
#' @param sep    Separator string used to build combined labels (default `"___"`).
#' @param ...    Extra arguments passed to `fun`.
#'
#' @noRd
#' @keywords internal
.reorder_within <- function(x,
                           by,
                           within,
                           fun = function(z) mean(z, na.rm = TRUE),
                           sep = "___",
                           ...) {
  if (length(x) != length(by) || length(x) != length(within)) {
    stop("`x`, `by`, and `within` must have the same length.", call. = FALSE)
  }

  # Coerce to character to avoid pasting "factor" levels silently
  x      <- as.character(x)
  within <- as.character(within)

  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = function(z) fun(z, ...))
}
