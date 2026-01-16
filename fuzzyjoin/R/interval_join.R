#' Join two tables based on overlapping (low, high) intervals
#'
#' Joins tables based on overlapping intervals: for example, joining
#' the row (1, 4) with (3, 6), but not with (5, 10). This operation is sped up
#' using interval trees as implemented in the IRanges package. You
#' can specify particular relationships between intervals (such as a maximum gap,
#' or a minimum overlap) through arguments passed on to
#' \code{\link[IRanges]{findOverlaps}}. See that documentation for descriptions
#' of such arguments.
#'
#' @param x A tbl
#' @param y A tbl
#' @param by Columns by which to join the two tables. If provided, this must be
#' two columns: start of interval, then end of interval
#' @param mode One of "inner", "left", "right", "full" "semi", or "anti"
#' @param ... Extra arguments passed on to \code{\link[IRanges]{findOverlaps}}
#'
#' @details This allows joining on date or datetime intervals. It throws an
#' error if the type of date/datetime disagrees between the two tables.
#'
#' This requires the IRanges package from Bioconductor. See here for installation:
#' \url{https://bioconductor.org/packages/release/bioc/html/IRanges.html}.
#'
#' @examples
#'
#' if (requireNamespace("IRanges", quietly = TRUE)) {
#'   x1 <- data.frame(id1 = 1:3, start = c(1, 5, 10), end = c(3, 7, 15))
#'   x2 <- data.frame(id2 = 1:3, start = c(2, 4, 16), end = c(4, 8, 20))
#'
#'   interval_inner_join(x1, x2)
#'
#'   # Allow them to be separated by a gap with a maximum:
#'   interval_inner_join(x1, x2, maxgap = 1)   # let 1 join with 2
#'   interval_inner_join(x1, x2, maxgap = 20)  # everything joins each other
#'
#'   # Require that they overlap by more than a particular amount
#'   interval_inner_join(x1, x2, minoverlap = 3)
#'
#'   # other types of joins:
#'   interval_full_join(x1, x2)
#'   interval_left_join(x1, x2)
#'   interval_right_join(x1, x2)
#'   interval_semi_join(x1, x2)
#'   interval_anti_join(x1, x2)
#' }
#'
#' @export
interval_join <- function(x, y, by, mode = "inner", ...) {
  if (!requireNamespace("IRanges", quietly = TRUE)) {
    stop("interval_join requires the IRanges package: ",
         "https://bioconductor.org/packages/release/bioc/html/IRanges.html")
  }

  f <- function(x, y) {
    if (ncol(x) != 2) {
      stop("interval_join must join on exactly two columns (start and end)")
    }

    # can join integer/numeric columns, but avoid any other class conflicts
    for (i in 1:2) {
      cx <- class(x[[i]])[1]
      cy <- class(y[[i]])[1]

      if (cx != cy &&
          !(cx %in% c("numeric", "integer") && cy %in% c("numeric", "integer"))) {
        stop("Cannot join columns of class ", cx, " and ", cy)
      }
    }

    r1 <- IRanges::IRanges(as.numeric(x[[1]]), as.numeric(x[[2]]))
    r2 <- IRanges::IRanges(as.numeric(y[[1]]), as.numeric(y[[2]]))

    ret <- as.data.frame(IRanges::findOverlaps(r1, r2, ...))
    names(ret) <- c("x", "y")
    ret
  }

  fuzzy_join(x, y, multi_by = by, index_match_fun = f, mode = mode)
}


#' @rdname interval_join
#' @export
interval_inner_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "inner", ...)
}


#' @rdname interval_join
#' @export
interval_left_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "left", ...)
}


#' @rdname interval_join
#' @export
interval_right_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "right", ...)
}


#' @rdname interval_join
#' @export
interval_full_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "full", ...)
}


#' @rdname interval_join
#' @export
interval_semi_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "semi", ...)
}


#' @rdname interval_join
#' @export
interval_anti_join <- function(x, y, by = NULL, ...) {
  interval_join(x, y, by, mode = "anti", ...)
}
