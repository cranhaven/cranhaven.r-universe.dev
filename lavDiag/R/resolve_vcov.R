#' @noRd
#' @keywords internal
#'
# Resolve which covariance matrix of free parameters to use for delta-method
.resolve_vcov <- function(fit, vcov_type, vcov_override = NULL) {
  # If user provided a matrix, use it
  if (!is.null(vcov_override)) {
    if (!is.matrix(vcov_override)) stop("`vcov_override` must be a numeric matrix.", call. = FALSE)
    return(vcov_override)
  }
  # Helper: try several keys safely
  get_try <- function(key) {
    tryCatch(lavaan::lavInspect(fit, key), error = function(e) NULL)
  }

  if (identical(vcov_type, "none")) return(NULL)

  if (identical(vcov_type, "default")) {
    V <- get_try("vcov")
    if (is.null(V)) stop("lavaan did not return `vcov`; cannot compute delta-method SEs.", call. = FALSE)
    return(V)
  }

  if (identical(vcov_type, "first.order")) {
    # some lavaan versions expose 1st order info/var under different keys; try several
    keys <- c("vcov.1st", "vcov.first.order", "first.order", "vcov")  # last is fallback
    for (k in keys) {
      V <- get_try(k)
      if (!is.null(V)) return(V)
    }
    warning("Could not obtain 'first.order' vcov; falling back to 'vcov'.")
    return(get_try("vcov"))
  }

  if (identical(vcov_type, "robust")) {
    # Try a few robust flavors; availability depends on estimator and version
    robust_keys <- c(
      "vcov.robust.sem",          # SEM robust (e.g., Yuan-Bentler)
      "vcov.robust",              # generic robust
      "vcov.robust.huber.white",  # Huber-White/sandwich
      "vcov.robust.satorra.bentler",
      "vcov"                      # fallback
    )
    for (k in robust_keys) {
      V <- get_try(k)
      if (!is.null(V)) {
        if (k == "vcov") warning("Robust vcov not available; using default 'vcov'.")
        return(V)
      }
    }
    stop("No robust vcov available and default 'vcov' missing; cannot compute SEs.", call. = FALSE)
  }

  # Fallback (should not get here)
  get_try("vcov")
}
