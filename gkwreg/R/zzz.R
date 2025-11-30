utils::globalVariables(c(
  "packageVersion", "dev.interactive", "calculateMean", "index", "theoretical",
  "observed", "lower", "upper", "log", "log_scale", "calculateResponseResiduals",
  "::", ":::", ".gkwreg_env", "get_tmb_info", "x", "Theoretical", "Empirical",
  "value", "loglik", "cook_dist", "fitted", "abs_resid", "leverage", "y_obs",
  "linpred", "resid", "model_label", "metric", "y", "Type", "Deviation", "object",
  "p_empirical", "p_theoretical", "statistic", "type", "Residual", "Family",
  "Value", "Criterion", "Parameter", "dbeta_", "dbkw", "dekw", "dgkw", "dkkw",
  "dkw dmc", "pbeta_", "pbkw", "pekw", "pgkw", "pkkw", "pkw", "pmc", "qbeta_",
  "qbkw", "qekw", "qgkw", "qkkw", "qkw", "qmc", "rbeta_", "rbkw", "rekw", "rgkw",
  "rkkw", "rkw", "rmc", "modifyList", "quantile", "rnorm", "setNames"
))

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


## usethis namespace: start
#' @importFrom Rcpp sourceCpp evalCpp
#' @import RcppArmadillo
#' @import graphics
#' @import gkwdist
## usethis namespace: end
NULL

#' @useDynLib gkwreg, .registration = TRUE
NULL


## usethis namespace: start
#' @importFrom numDeriv grad hessian
## usethis namespace: end
NULL


# # Internal package environment (defined here for completeness)
# # This is also defined in gkwreg-compile-tmb.R but that's fine -
# # the first definition wins
# .gkwreg_env <- new.env(parent = emptyenv())
#
#
# .onLoad <- function(libname, pkgname) {
#   # Attempt to load DLL
#   # In development (devtools::load_all), DLL is already loaded
#   # In normal installation, needs explicit loading
#   dll_loaded <- tryCatch({
#     library.dynam("gkwreg", pkgname, libname)
#     TRUE
#   }, error = function(e) {
#     # If it fails, check if already loaded (development environment)
#     "gkwreg" %in% names(getLoadedDLLs())
#   })
#
#   if (!dll_loaded) {
#     warning("gkwreg DLL could not be loaded. Compiled functions may not work.")
#   }
#
#   assign("pkg_name", pkgname, envir = .gkwreg_env)
#   assign("lib_name", libname, envir = .gkwreg_env)
#   assign("gkwregPredictResid", character(0), envir = .gkwreg_env)
#
#   invisible()
# }
#
# .onUnload <- function(libpath) {
#   # Only attempt to unload if DLL is loaded
#   if ("gkwreg" %in% names(getLoadedDLLs())) {
#     library.dynam.unload("gkwreg", libpath)
#   }
# }
