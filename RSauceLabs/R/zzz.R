.onLoad <- function(libname = find.package("RSauceLabs"), pkgname = "RSauceLabs"){
  op <- options()
  op.RSauceLabs <- list(

  )
  toset <- !(names(op.RSauceLabs) %in% names(op))
  if(any(toset)) options(op.RSauceLabs[toset])
  invisible()
}
utils::globalVariables("supported_backend_version") # spurious message
