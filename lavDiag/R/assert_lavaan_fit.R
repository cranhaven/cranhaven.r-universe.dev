#' Assert that `fit` is a usable lavaan/blavaan model
#'
#' @param fit A fitted lavaan/blavaan object.
#' @param require_converged logical; if TRUE, fail on non-converged models.
#' @param require_meanstructure logical|NA; TRUE = must have meanstructure,
#'        FALSE = must NOT have meanstructure, NA = don't check.
#' @param require_latent logical; if TRUE, require at least one latent variable.
#' @return Invisibly returns NULL if all checks pass; otherwise `stop()`.
#' @noRd
#' @keywords internal
.assert_lavaan_fit <- function(fit,
                               require_converged     = TRUE,
                               require_meanstructure = NA,
                               require_latent        = FALSE,
                               require_measurement   = c("none", "lambda", "lambda+nu"),
                               forbid_multilevel     = TRUE) {
  # -- Class check -------------------------------------------------------------
  if (!inherits(fit, "lavaan")) {
    cls <- paste(class(fit), collapse = ", ")
    stop("`fit` must be a fitted lavaan/blavaan object; received: ", cls, call. = FALSE)
  }

  # -- Convergence -------------------------------------------------------------
  if (isTRUE(require_converged)) {
    conv <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) NA)
    if (!isTRUE(conv)) {
      msg <- tryCatch(lavaan::lavInspect(fit, "optim")$message, error = function(e) NULL)
      if (is.null(msg) || !nzchar(msg)) msg <- "Model did not converge."
      stop("lavaan convergence check failed: ", msg, call. = FALSE)
    }
  }

  # -- Meanstructure -----------------------------------------------------------
  if (!is.na(require_meanstructure)) {
    has_mean <- isTRUE(tryCatch(lavaan::lavInspect(fit, "meanstructure"), error = function(e) FALSE))
    if (require_meanstructure && !has_mean) {
      stop("Model was not fitted with meanstructure = TRUE, but it is required.", call. = FALSE)
    }
    if (!require_meanstructure && has_mean) {
      stop("Model has meanstructure = TRUE, but it is not expected here.", call. = FALSE)
    }
  }

  # -- Latent variables --------------------------------------------------------
  if (isTRUE(require_latent)) {
    lv <- tryCatch(lavaan::lavNames(fit, "lv"), error = function(e) character())
    if (length(lv) == 0L) stop("No latent variables detected in `fit`.", call. = FALSE)
  }

  # -- Multilevel (optional forbid) -------------------------------------------
  if (isTRUE(forbid_multilevel)) {
    n_levels <- tryCatch(lavaan::lavInspect(fit, "nlevels"), error = function(e) NA_integer_)
    if (!is.na(n_levels) && n_levels >= 2L) {
      stop("Multilevel models not supported yet.", call. = FALSE)
    }
  }

  # -- Measurement pieces from lavInspect(..., "est") --------------------------
  req <- match.arg(require_measurement)
  if (req != "none") {
    est <- tryCatch(lavaan::lavInspect(fit, "est"), error = function(e) NULL)
    if (is.null(est)) stop("Could not extract parameter estimates via lavInspect(fit, 'est').", call. = FALSE)

    # Normalize to list-per-group:
    est_list <- if (is.list(est) && all(c("lambda","nu") %in% names(est)) || "lambda" %in% names(est)) {
      list(est)
    } else if (is.list(est)) {
      est
    } else {
      stop("Unexpected structure of lavInspect(fit, 'est').", call. = FALSE)
    }

    # Check lambda presence and dimensions
    L_ok <- vapply(est_list, function(e) is.matrix(e$lambda) && nrow(e$lambda) > 0 && ncol(e$lambda) > 0, TRUE)
    if (!all(L_ok)) stop("Loading matrix (Lambda) missing or has zero rows/cols for some group.", call. = FALSE)

    if (req == "lambda+nu") {
      nu_ok <- vapply(est_list, function(e) !is.null(e$nu) && length(e$nu) > 0, TRUE)
      if (!all(nu_ok)) {
        stop("Intercepts (nu) are required but not present in lavInspect(fit, 'est'). Fit with meanstructure = TRUE.", call. = FALSE)
      }
    }
  }

  invisible(NULL)
}
