# Fiscal Risks and Sustainability Report.
#
# get_pension_projections() was deprecated in 0.5.1. In July 2026 the OBR
# restructured the FSR workbook set: the executive-summary sheet this
# function parsed ("C1.2", state pension spending under demographic and
# triple-lock scenarios) no longer exists. The equivalent series now sits in
# the Chapter 3 "Long-term spending projections" workbook (Chart 3.11,
# "State pension spending under alternative uprating assumptions"), with a
# different scenario structure. The function is kept as a stub so existing
# scripts do not fail with "object not found"; the download and parsing code
# has been removed.

#' Get long-run state pension spending projections (deprecated)
#'
#' @description
#' **Deprecated since obr 0.5.1.**
#'
#' In July 2026 the OBR restructured the Fiscal Risks and Sustainability
#' Report workbook set. The executive-summary sheet this function read
#' (`C1.2`, state pension spending split into demographic and triple-lock
#' scenarios) no longer exists, so the function could no longer return data.
#'
#' The equivalent series is now published in the FSR Chapter 3 "Long-term
#' spending projections" workbook, as Chart 3.11 "State pension spending
#' under alternative uprating assumptions", with a different scenario
#' structure (triple-lock, CPI, and average-earnings uprating rather than the
#' old demographic vs triple-lock split). See
#' <https://obr.uk/frs/fiscal-risks-and-sustainability-july-2026/>.
#'
#' This stub is retained so existing scripts do not error. It emits a
#' deprecation warning and returns `NULL`. It will be removed in a future
#' release.
#'
#' @param refresh Ignored. Retained so existing calls do not error.
#'
#' @return `NULL`, invisibly.
#'
#' @examples
#' # Deprecated since 0.5.1: emits a warning and returns NULL.
#' suppressWarnings(get_pension_projections())
#'
#' @family long-term fiscal
#' @export
get_pension_projections <- function(refresh = FALSE) {
  .Deprecated(
    msg = paste0(
      "`get_pension_projections()` is deprecated and now returns NULL.\n",
      "The OBR restructured the Fiscal Risks and Sustainability Report in ",
      "July 2026; the state pension spending scenarios this function read ",
      "(executive-summary sheet 'C1.2') are no longer published in that ",
      "form. The equivalent series now sits in the FSR Chapter 3 workbook ",
      "(Chart 3.11, 'State pension spending under alternative uprating ",
      "assumptions'). See ",
      "https://obr.uk/frs/fiscal-risks-and-sustainability-july-2026/."
    )
  )
  invisible(NULL)
}
