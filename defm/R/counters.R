#' @export
print.DEFM_counters <- function(x, ...) {
  print_defm_counters(x)
}

#' @export
print.DEFM_counter <- function(x, ...) {
  print_defm_counter(x)
}

#' @rdname get_counters
#' @export
as.list.DEFM_counters <- function(x, ...) {

  n_counters <- length(x)
  lapply(seq_len(n_counters) - 1, \(i) {
    as.list(x[i])
  })

}

#' @export
#' @rdname get_counters
#' @param x Either a `DEFM_counters` or a `DEFM_counter` object.
#' @param ... Further arguments passed to the method (not used).
#' @returns
#' - The `as.list` methods return a list with the name and description
#' of the counters.
as.list.DEFM_counter <- function(x, ...) {

  as_list_defm_counter(x)

}

#' @export
#' @rdname get_counters
#' @param model A [DEFM] model object.
#' @returns
#' - The function `set_counters_names()` returns the counters invisibly.
set_counters_names <- function(x, ...) UseMethod("set_counters_names")

#' @export
#' @rdname get_counters
set_counters_names.DEFM <- function(x, ...) {
  set_counters_names(get_counters(x), ...)
  invisible(x)
}

#' @export
#' @rdname get_counters
set_counters_names.DEFM_counters <- function(
  x, ...
) {

  dots <- list(...)

  args_positions <- names(dots)
  tmp_test <- which(args_positions == "")
  if (length(tmp_test) > 0L) {
    stop(
       "Arguments passed via `...` should be named. ",
       "The following positions were empty (\"\"): ",
       paste(tmp_test, collapse = ", ")
      )
  }

  # Figuring out the positions (can be turned integers)
  args_positions <- suppressWarnings({
    as.integer(args_positions)
  })

  tmp_test <- which(is.na(args_positions))
  if (length(tmp_test)) {
    stop(
      "Arguments passed via `...` should be named with integers ",
      "indicating the position of the counter to modify. ",
      "The following positions were not integers: ",
      paste(tmp_test, collapse = ", ")
    )
  }


  n_counters <- length(x)
  if (any(args_positions < 0L) || any(args_positions >= n_counters)) {
    stop(
      "Arguments passed via `...` should be named with integers ",
      "indicating the position of the counter to modify. ",
      "The valid range is from 0 to ", n_counters - 1L, "."
    )
  }

  # Getting the counters
  Map(\(pos, new_name) {
    set_counter_info(
      x[pos],
      new_name = new_name
    )
  }, pos = args_positions, new_name = unlist(dots))

  invisible(x)

}