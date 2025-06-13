#' @name collapse
#' @author Sven E. Templer
#' @title Collapse objects
#' @description 
#' Collapse objects as in the \code{paste} function.
#' @param x Any R object.
#' @param sep A character string to separate value columns.
#' \code{NULL} retains a vector.
#' @param by Column names to split data frame by, before
#' applying collapse on each remaining column within each piece.
#' Using the default (all columns), then \code{unique(x)} is
#' returned. Columns can be specified by names or integer
#' with the column numbers. Using \code{0} or \code{NULL}
#' collapses all columns.
#' @param ... Forwarded to or from other methods.
#' @param .unique Logical, return only unique values.
#' @param .sort Logical, sort the values.
#' @param .decreasing Logical, if sorting, then by decreasing values.
#' @param .unlist Logical, if value columns need to be unlisted
#' before collapsing.
#' @param .sortby Logical, sort the output on the \code{by} columns.
#' This applies ,
#' If \code{x} was a \code{data.table}, then the keys are set as
#' the \code{by} values.
#' @details 
#' For the \code{data.frame} method, \code{x} is converted to a 
#' \code{data.table} before applying the piece- and column-wise collapses. If
#' the input is already inheriting from \code{data.table}, then the class is
#' retained.\cr
#' \code{.sortby} is causing \code{setkeyv(x, by)} to be applied to \code{x}
#' after converting to a \code{data.table}.
#' @examples
#' #
#' 
#' ### some data
#' 
#' set.seed(12)
#' s <- s2 <- sample(LETTERS[1:4], 9, replace = TRUE)
#' s2[1:2] <- rev(s2[1:2])
#' d <- data.frame(group = rep(letters[c(3,1,2)], each = 3), 
#'                 value = s,
#'                 level = factor(s2),
#'                 stringsAsFactors = FALSE)
#' 
#' ### collapse vectors
#' 
#' collapse(letters)
#' collapse(1:3)               # coerced to character
#' collapse(LETTERS[1:5], "-") # separated by '-'
#'
#' ### collapse data.frames
#' 
#' # by all columns (same as unique)
#' collapse(d)
#' # by a grouping column
#' collapse(d, by = 1)
#' # by multiple, but not all columns
#' collapse(d, by = c("group", "value"))
#' # return single row
#' collapse(d, by = 0)
#' # return single row, unique and sorted values
#' collapse(d, by = 0, .unique = TRUE, .sort = TRUE)
#' 
#' #

#' @rdname collapse
#' @export
collapse <- function (x, sep, ...) { UseMethod("collapse") }

#' @rdname collapse
#' @export
collapse.default <- function (x, sep = "", ..., .unique = FALSE, .sort = FALSE, .decreasing = FALSE) {
  if (.unique) 
    x <- unique(x)
  if (.sort)
    x <- sort(x, decreasing = .decreasing)
  if (is.null(sep)) 
    return(x)
  x <- paste(x, collapse = sep)
  return(x)
}

collapse_frame <- function (x, sep, .unique, .sort, .decreasing, .unlist)
{
  lapply(x, function (y) {
    if (.unlist)
      y <- unlist(y, use.names = FALSE)
    y <- collapse(y, sep = sep, .unique = .unique, .sort = .sort, .decreasing = .decreasing)
    if (is.null(sep))
      y <- list(y)
    return(y)
  })
} 

#' @rdname collapse
#' @export
collapse.data.frame <- function (x, sep = "", by = names(x), ..., .unique = FALSE, .sort = FALSE, .decreasing = FALSE, .unlist = FALSE, .sortby = FALSE)
{
  if (is.null(names(x)))
    names(x) <- seq_along(x)
  names(x) <- make.names(names(x), unique = TRUE)
  if (!is.character(by))
    by <- names(x)[by]
  val <- setdiff(names(x), by)
  is.dt <- inherits(x, "data.table")
  # microbenchmark 
  # `[.data.table(x, by = by)` or `[.data.table(x, keyby = by)` 
  # vs 
  # `data.table(x, key = by)
  x <- as.data.table(x)
  if (.sortby)
    setkeyv(x, by)
  x <- if (length(val))
    x[, collapse_frame(.SD, sep, .unique, .sort, .decreasing, .unlist), by = by]
  else
    unique(x)
  if (!is.dt)
    x <- as.data.frame(x)
  return(x)
}

