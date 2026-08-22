# Stability-rule margin ("headroom") derived from EFO Table 6.5.
#
# The Charter's stability rule requires the current budget to be in balance
# or surplus by the target year. Table 6.5 (Components of net borrowing)
# publishes the current budget deficit for every year of the forecast, so
# the margin against the rule in any year is simply the current budget
# surplus (the deficit with its sign flipped). This function returns that
# path. It deliberately does not hardcode which year is the target year:
# the Charter's target-year convention changes over time (5th forecast
# year, rolling to 3rd from 2026-27), so the caller supplies it.

#' Compute the stability-rule margin from the EFO forecast
#'
#' Returns the current budget surplus for every year of an EFO forecast,
#' derived from Table 6.5 (Components of net borrowing). Under the Charter
#' for Budget Responsibility's stability rule the current budget must be in
#' balance or surplus by the target year, so the value in the target year
#' is the margin, or "headroom", against the rule: positive means the rule
#' is met with room to spare, negative means it is missed.
#'
#' @details
#' The value returned is `-1 *` the published "Current budget deficit"
#' series, i.e. a surplus is positive. Pass `target_year` to flag the year
#' the rule currently bites on; the function does not guess it, because the
#' Charter's target-year convention changes over time (see
#' [obr_fiscal_rules()] for the rules as encoded at release).
#'
#' Note the OBR's own published headroom figure at a fiscal event can
#' differ slightly from the Table 6.5 arithmetic (rounding, and any
#' rule-specific adjustments described in the EFO text). Treat this as the
#' published forecast path for the rule metric, not a reproduction of the
#' OBR's press-notice headroom number.
#'
#' @param vintage Optional EFO vintage label such as `"March 2026"`. If
#'   `NULL` (the default), uses any pin set via [obr_pin()] or the latest
#'   live EFO.
#' @param target_year Optional fiscal-year string (e.g. `"2029-30"`). If
#'   supplied, an `is_target_year` column flags that year.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard schema columns (`period`,
#' `period_type`, `series`, `metric_type`, `value`, `unit`), where `series`
#' is `"Current budget surplus"` and `value` is in GBP billion (positive =
#' surplus = headroom under the stability rule). If `target_year` is
#' supplied, an additional logical `is_target_year` column is included.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#'
#' hr <- tryCatch(obr_headroom(), error = function(e) NULL)
#' if (!is.null(hr)) hr
#'
#' # Flag the target year and read off the margin
#' hr <- tryCatch(obr_headroom(target_year = "2029-30"),
#'                error = function(e) NULL)
#' if (!is.null(hr)) hr[hr$is_target_year, ]
#'
#' options(op)
#' }
#'
#' @family fiscal rules
#' @export
obr_headroom <- function(vintage = NULL, target_year = NULL, refresh = FALSE) {
  if (!is.null(target_year) &&
      (!is.character(target_year) || length(target_year) != 1L ||
       !grepl("^[0-9]{4}-[0-9]{2}$", target_year))) {
    cli::cli_abort("{.arg target_year} must be a fiscal-year string like {.val 2029-30}.")
  }

  efo  <- get_efo_table("6.5", vintage = vintage, refresh = refresh)
  prov <- obr_provenance(efo)

  df <- as.data.frame(efo)
  cb <- df[df$series == "Current budget deficit", , drop = FALSE]
  if (nrow(cb) == 0L) {
    cli::cli_abort(c(
      "Could not find the {.val Current budget deficit} series in EFO Table 6.5.",
      "!" = "The OBR may have renamed the series. Please file an issue at https://github.com/charlescoverdale/obr/issues."
    ))
  }

  out <- data.frame(
    period      = cb$period,
    period_type = cb$period_type,
    series      = "Current budget surplus",
    metric_type = "level",
    value       = -cb$value,
    unit        = "gbp_bn",
    stringsAsFactors = FALSE
  )
  out <- out[order(out$period), ]
  rownames(out) <- NULL

  notes <- paste0("Derived from EFO Table 6.5: value = -(Current budget ",
                  "deficit). Positive = surplus = margin against the ",
                  "Charter stability rule.")
  if (!is.null(target_year)) {
    out$is_target_year <- out$period == target_year
    if (!any(out$is_target_year)) {
      cli::cli_warn(c(
        "{.arg target_year} {.val {target_year}} is not a year in this EFO forecast.",
        "i" = "Forecast years: {.val {unique(out$period)}}."
      ))
    }
    notes <- paste0(notes, sprintf(" Target year flagged: %s.", target_year))
  }

  new_obr_tbl(
    data        = out,
    publication = prov$publication,
    vintage     = prov$vintage,
    source_url  = prov$source_url,
    retrieved   = prov$retrieved,
    file_md5    = prov$file_md5,
    notes       = notes
  )
}
