#' Check if a lavaan model is single-group
#'
#' This helper function checks whether a fitted \code{lavaan} model
#' contains only a single group.
#'
#' @param fit An object of class \code{lavaan}.
#'
#' @return Logical scalar. Returns \code{TRUE} if the model has only
#'   one group, \code{FALSE} otherwise.
#'
#' @noRd
#' @keywords internal
.is_single_group <- function(fit) {
  # -- Validate input ----------------------------------------------------------
  .assert_lavaan_fit(fit)

  # -- Primary: use ngroups from lavInspect (most reliable) --------------------
  ng <- tryCatch(lavaan::lavInspect(fit, "ngroups"),
                 error = function(e) NA_integer_)

  if (!is.na(ng)) return(ng <= 1L)

  # -- Fallback: use group labels if ngroups unavailable -----------------------
  gl <- tryCatch(lavaan::lavInspect(fit, "group.label"),
                 error = function(e) NULL)

  if (!is.null(gl)) return(length(gl) <= 1L)

  # -- Conservative default: assume single-group if inspection failed ----------
  TRUE
}
