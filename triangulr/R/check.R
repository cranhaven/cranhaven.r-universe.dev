check_func <- function(..., .f) {
  all(sapply(list(...), .f))
}

check_numeric <- function(...) {
  if (!check_func(..., .f = is.numeric)) {
    cnd_signal(tri_error_numeric(...))
  }
}

check_scalar_lgl <- function(...) {
  if (!check_func(
    ...,
    .f = function(x) {
      length(x) == 1 && is.logical(x)
    }
  )) {
    if (length(list(...)) == 1) {
      cnd_signal(tri_error_logical(...))
    } else {
      cnd_signal(tri_error_logical2(...))
    }
  }
}

is_scalar <- function(...) {
  check_func(
    ...,
    .f = function(x) {
      length(x) == 1
    }
  )
}

try_recycle <- function(f, ..., .f = length) {
  p <- tryCatch({
    vec_recycle_common(..., .size = .f(f))
  }, error = function(c) {
    cnd_signal(tri_error_recycle(f, ...))
  })
  p_nm <- sapply(substitute(list(...)), deparse)[-1]
  names(p) <- p_nm
  list2env(p, envir = parent.frame())
}
