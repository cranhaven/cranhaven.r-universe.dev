string_as_set <- function(str) {
    if (!is.null(str)) {
        sprintf("{%s}", paste0(str, collapse = ", "))
    }
}


assert_named_list <- function(x, types = NULL) {

    if (!inherits(x, c("list", "environment"))) {
      stop("'x' must be a list or environment")
    }

    if (is.list(x) && (is.null(nms <- names(x)) || anyDuplicated(nms))) {
      stop("'x' must have unique names")
    }

    if (!is.null(types)) {
      if (!all(vapply(x, inherits, logical(1), what = types))) {
        stop(sprintf(
          "All elements of 'x' should inherit from %s",
          string_as_set(types)
        ))
      }
    }

  invisible(x)
}
