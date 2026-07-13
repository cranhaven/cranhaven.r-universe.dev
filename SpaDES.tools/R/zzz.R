## -------------------------------------------------------------------------- ##
## edit the package options help documentation in spades-tools-package.R      ##
## -------------------------------------------------------------------------- ##

.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list( # nolint
    spades.lowMemory = FALSE
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  ## import functions using backports:
  backports::import(pkgname, "isFALSE")

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.tools version ", packageVersion("SpaDES.tools"), ".")
  }
}
