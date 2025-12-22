.onLoad <- function(libname, pkgname) {

  # Default options --------------------------------------------------------
  op <- options()
  defaults <- list(
    mknapsack.solver = "lpsolve"
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  invisible()
}
