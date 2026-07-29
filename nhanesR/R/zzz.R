# R/zzz.R
# Package load hook for nhanesR
# Cache directory management lives in nhanes_cache.R
# HTTP helpers live in nhanes_http.R

# Suppress R CMD CHECK "no visible binding" notes for internal sysdata objects.
utils::globalVariables(c(
  ".nhanes_cycles", ".nhanes_iii",
  ".lmf_registry",  ".lmf_colspec", ".ucod_labels",
  ".early_biopro_catalog"
))

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  op <- options()
  op_nhanesR <- list(
    nhanesR.cache_dir = file.path(tempdir(), "nhanesR"),
    nhanesR.verbose   = TRUE,
    nhanesR.timeout   = 120L
  )
  toset <- !(names(op_nhanesR) %in% names(op))
  if (any(toset)) options(op_nhanesR[toset])
  invisible()
}
