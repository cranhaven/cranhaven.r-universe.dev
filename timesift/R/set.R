#' Several built representations of the same targets
#'
#' A named list of arrays covering the same targets and differing only in how the record was
#' reduced: several calendar grains, several lookback spans, a block of features beside a
#' sequence. It is keyed by the label each representation is reported under, which is what
#' [grain_ladder()] fits across and what a candidate in a [timesift()] fit is named by.
#'
#' @param x A named list of built representations -- [grain_matrix()], [lookback_matrix()] or
#'   [feature_matrix()] results -- or a single one.
#'
#' @return A `timesift_set`: the list, keyed by representation label.
#'
#' @examples
#' t <- seq(as.POSIXct("2021-09-01", tz = "UTC"), by = "hour", length.out = 24 * 60)
#' d <- data.frame(plot = rep(c("a", "b"), each = length(t)), t = rep(t, 2),
#'                 temp = rnorm(2 * length(t)))
#' s <- grain_matrix(d, plot, t, temp, grain = c("day", "week", "month"))
#' s
#' names(s)
#'
#' @export
timesift_set <- function(x) {
  if (inherits(x, "timesift_matrix")) {
    x <- stats::setNames(list(x), attr(x, "grain"))
  }
  if (!is.list(x) || !length(x)) {
    stop("a timesift set is a non-empty list of built representations.", call. = FALSE)
  }
  if (is.null(names(x)) || anyDuplicated(names(x)) || any(!nzchar(names(x)))) {
    stop("every representation in a timesift set needs its own label.", call. = FALSE)
  }
  ok <- vapply(x, inherits, logical(1L), "timesift_matrix")
  if (!all(ok)) {
    stop("element", if (sum(!ok) > 1L) "s" else "", " ",
         paste(names(x)[!ok], collapse = ", "), " ",
         if (sum(!ok) > 1L) "are" else "is",
         " not a built representation.", call. = FALSE)
  }
  targets <- lapply(x, function(m) dimnames(m)[[1L]])
  same <- vapply(targets, identical, logical(1L), targets[[1L]])
  if (!all(same)) {
    stop("every representation in a set must cover the same targets; ",
         paste(names(x)[!same], collapse = ", "), " does not.", call. = FALSE)
  }
  structure(x, class = "timesift_set")
}

#' @export
print.timesift_set <- function(x, ...) {
  cat("<timesift set>", .plural(length(x), "representation"), "over",
      .plural(dim(x[[1L]])[1L], "target"), "\n")
  for (nm in names(x)) {
    m <- x[[nm]]
    cat(sprintf("  %-14s %5d bins x %d channels (%s)\n", nm, dim(m)[2L], dim(m)[3L],
                paste(attr(m, "stats"), collapse = ", ")))
  }
  invisible(x)
}

#' @export
`[.timesift_set` <- function(x, i) {
  timesift_set(NextMethod())
}

# Every entry point that fits across representations takes a matrix, a set, or a bare named list,
# and works on a set. One coercion, so no caller repeats the three cases.
.as_set <- function(x) {
  if (inherits(x, "timesift_set")) {
    return(x)
  }
  timesift_set(x)
}
