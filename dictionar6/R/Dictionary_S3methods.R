#' @export
`[.Dictionary` <- function(x, i) {
  x$get_list(i)
}

#' @export
`[<-.Dictionary` <- function(x, i, value) { # nolint
  x$add(keys = i, values = value)
  invisible(x)
}

#' @export
length.Dictionary <- function(x) {
  x$length
}

#' @export
summary.Dictionary <- function(object, n = 2, ...) {
  object$summary(n = n)
}

#' @export
as.character.Dictionary <- function(x, n = 2, ...) { # nolint
  keys <- x$keys
  values <- vapply(x$values, as.character, character(1), USE.NAMES = FALSE)

  lng <- x$length
  if (lng > (2 * n)) {
    string <- paste0(
      paste(keys[1:n], values[1:n],
        sep = ": ",
        collapse = ", "
      ),
      ", ..., ", paste(keys[(lng - n + 1):lng],
        values[(lng - n + 1):lng],
        sep = ": ", collapse = ", "
      )
    )
  } else {
    string <- paste(keys, values, sep = ": ", collapse = ", ")
  }

  sprintf("{%s}", string)
}

#' @export
c.Dictionary <- function(...) {
  x <- list(...)
  types <- vapply(x, function(.x) list(.x$typed, length(.x$types), .x$types),
                  vector("list", 3))
  # different typing
  if (length(unique(types[1, ])) > 1) {
    stop("Can only combine Dictionaries if all typed or all untyped.")
    # all typed or untyped
  } else {
    # untyped
    if (!unlist(types[1, 1])) {
      Dictionary$new(x = unlist(lapply(x, "[[", "items"), FALSE))
      # typed
    } else {
      # different type lengths
      if (length(unique(types[2, ])) > 1) {
        stop("Can only combine typed Dictionaries of the same type(s).")
      } else {
        if (length(unique(unlist(types[3, ]))) != types[2, 1][[1]]) {
          stop("Can only combine typed Dictionaries of the same type(s).")
        } else {
          Dictionary$new(x = unlist(lapply(x, "[[", "items"), FALSE),
                         types = unlist(types[3, 1]))
        }
      }
    }
  }
}
