.onLoad <- function(libname, pkgname) {
  op <- options()
  op.nisrarr <- list(
    nisrarr.cache_max_age = 60 * 60,
    nisrarr.throttle = 60
  )

  toset <- !(names(op.nisrarr) %in% names(op))
  if (any(toset)) {
    options(op.nisrarr[toset])
  }
}
