.rxode2parseVersion <- utils::packageVersion("rxode2parse")
.onLoad <- function(libname, pkgname) {
  if (!identical(.rxode2parseVersion, utils::packageVersion("rxode2parse"))) {
    stop("rxode2random compiled with rxode2parse '", as.character(.rxode2parseVersion),
         "' but rxode2parse '", as.character(utils::packageVersion("rxode2parse")),
         "' is loaded\nRecompile rxode2 with the this version of rxode2parse",
         call. = FALSE
         )
  } else {
    requireNamespace("rxode2parse", quietly=TRUE)
  }
}
