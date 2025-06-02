.onLoad <- function(libname, pkgname) { # nocov start

  op <- options()
  op_mpt <- list(
    MPTmultiverse = set_default_options()
  )
  
  toset <- !(names(op_mpt) %in% names(op))
  if(any(toset)) options(op_mpt[toset])

  invisible()
} # nocov end
