.dparserVersion <- utils::packageVersion("dparser")

## nocov start
.onLoad <- function(libname, pkgname) {
  if (!identical(.dparserVersion, utils::packageVersion("dparser"))) {
    stop("rxode2parse compiled with dparser '", as.character(.dparserVersion),
      "' but dparser '", as.character(utils::packageVersion("dparser")),
      "' is loaded\nRecompile rxode2parse with the this version of dparser",
      call. = FALSE
    )
  } else {
    requireNamespace("dparser", quietly=TRUE)
  }
  requireNamespace("qs", quietly=TRUE)
  requireNamespace("Rcpp", quietly=TRUE)
  rxode2parseAssignTranslation(.parseEnv$.rxode2parseDf)
}
## nocov end
