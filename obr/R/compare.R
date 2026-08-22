# Workflow helpers added in v0.4.0.
# obr_compare_vintages():    diff two EFO vintages on the standard schema.
# obr_actual_vs_forecast():  pair OBR forecasts with ONS outturn from the PFD.

# Internal: dispatch a "what" key to the underlying data function. Named
# shortcuts cover the four original v0.4.0 comparisons; any other value is
# treated as an EFO catalogue table id (v0.6.0), so all 39 detailed-forecast
# tables can be diffed across vintages.
.compare_fn <- function(what) {
  switch(
    what,
    "fiscal"     = function(...) get_efo_fiscal(...),
    "inflation"  = function(...) get_efo_economy("inflation", ...),
    "labour"     = function(...) get_efo_economy("labour", ...),
    "output_gap" = function(...) get_efo_economy("output_gap", ...),
    {
      cat_ids <- efo_catalogue_table()$table_id
      if (!what %in% cat_ids) {
        cli::cli_abort(c(
          "Unknown {.arg what} value: {.val {what}}.",
          "i" = paste0("Use one of {.val fiscal}, {.val inflation}, ",
                       "{.val labour}, {.val output_gap}, or any table id ",
                       "from {.fn obr_efo_catalogue} (e.g. {.val 6.13}).")
        ))
      }
      function(...) get_efo_table(what, ...)
    }
  )
}

#' Compare two EFO vintages
#'
#' Pulls the same EFO table from two vintages and returns a tidy diff with
#' a revision column (`value_b - value_a`). Useful for quantifying how the
#' OBR's view changed between fiscal events.
#'
#' @details
#' Rows are the **inner join** of the two vintages on the schema keys
#' (`period`, `period_type`, `series`, `metric_type`, `unit`). Periods or
#' series that are present in only one vintage are silently dropped. If
#' you need to see what was added or removed between vintages, compare
#' `obr_efo_vintages()` row counts or call the underlying functions
#' directly with each vintage and `setdiff()` on the keys.
#'
#' Calling the function with `vintage_a == vintage_b` is allowed and
#' returns an all-zero `revision` column. There is no special handling
#' beyond that.
#'
#' @param vintage_a,vintage_b EFO vintage labels (e.g. `"October 2024"`,
#'   `"March 2026"`). Use [obr_efo_vintages()] to see all valid labels.
#' @param what Which EFO table to compare. Either one of the named
#'   shortcuts `"fiscal"` (Table 6.5, the default), `"inflation"` (sheet
#'   1.7), `"labour"` (sheet 1.6), `"output_gap"` (sheet 1.14), or any
#'   table id from [obr_efo_catalogue()] (e.g. `"6.13"`, `"1.19"`), so all
#'   detailed-forecast tables can be diffed across vintages.
#' @param refresh Logical. If `TRUE`, re-download even if cached files
#'   exist. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema columns
#' (`period`, `period_type`, `series`, `metric_type`, `unit`) plus
#' `value_a`, `value_b`, and `revision` (`value_b - value_a`).
#' Provenance points at the second vintage; the first vintage URL is
#' recorded in the `notes` field.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' diff <- obr_compare_vintages("October 2024", "March 2026")
#' diff[diff$series == "Net borrowing", ]
#'
#' # Compare the inflation forecast across two vintages
#' inf_diff <- obr_compare_vintages("October 2024", "March 2026",
#'                                  what = "inflation")
#'
#' # Any catalogue table works too, e.g. debt interest (Table 6.16)
#' di_diff <- obr_compare_vintages("November 2025", "March 2026",
#'                                 what = "6.16")
#' options(op)
#' }
#'
#' @family forecasts
#' @export
obr_compare_vintages <- function(vintage_a, vintage_b,
                                 what = "fiscal",
                                 refresh = FALSE) {
  if (!is.character(what) || length(what) != 1L || is.na(what)) {
    cli::cli_abort("{.arg what} must be a single character string.")
  }
  fn <- .compare_fn(what)

  a <- fn(vintage = vintage_a, refresh = refresh)
  b <- fn(vintage = vintage_b, refresh = refresh)
  if (is.null(a) || is.null(b)) {
    cli::cli_abort(c(
      "Table {.val {what}} could not be fetched for both vintages.",
      "i" = "Cross-reference sheets (see {.fn obr_efo_catalogue}) cannot be compared directly."
    ))
  }

  prov_b  <- obr_provenance(b)
  prov_a  <- obr_provenance(a)

  keys <- c("period", "period_type", "series", "metric_type", "unit")
  # Some tables carry extra identifying columns (e.g. sub_sector for 6.4);
  # include any that appear in both vintages so the join stays one-to-one.
  extra <- setdiff(intersect(names(a), names(b)), c(keys, "value"))
  keys  <- c(keys, extra)
  out  <- merge(
    as.data.frame(a)[, c(keys, "value")],
    as.data.frame(b)[, c(keys, "value")],
    by        = keys,
    suffixes  = c("_a", "_b"),
    all       = FALSE
  )
  out$revision <- out$value_b - out$value_a
  rownames(out) <- NULL

  new_obr_tbl(
    data        = out,
    publication = prov_b$publication,
    vintage     = prov_b$vintage,
    source_url  = prov_b$source_url,
    retrieved   = prov_b$retrieved,
    file_md5    = prov_b$file_md5,
    notes       = sprintf(
      paste0("Vintage diff: %s (a) -> %s (b). ",
             "revision = value_b - value_a. ",
             "Earlier vintage URL: %s"),
      prov_a$vintage, prov_b$vintage, prov_a$source_url
    )
  )
}

# Internal: map a get_forecasts() series name to the matching PFD outturn fn.
.actuals_fn_for <- function(series) {
  switch(
    series,
    "PSNB"        = function(...) get_psnb(...),
    "PSND"        = function(...) get_psnd(...),
    "expenditure" = function(...) get_expenditure(...),
    NULL
  )
}

#' Pair OBR forecasts with PFD outturn
#'
#' Joins the long-format Historical Forecasts Database for a given series
#' against the Public Finances Databank outturn for the same series.
#' Returns one row per (forecast vintage, fiscal year) where both an OBR
#' forecast value and an ONS outturn value exist, with the forecast error
#' (`value_forecast - value_actual`) computed.
#'
#' Useful for forecast-evaluation studies, similar in shape to the OBR's
#' own Forecast Evaluation Report decomposition.
#'
#' Currently supports series for which a clean `gbp_bn` outturn function
#' exists in this package: `"PSNB"`, `"PSND"`, `"expenditure"`. Other
#' series (CPI, GDP, percentages of GDP) need outturn from external
#' packages and will error.
#'
#' @param series Forecast series. One of `"PSNB"`, `"PSND"`, `"expenditure"`.
#'   Defaults to `"PSNB"`.
#' @param refresh Logical. If `TRUE`, re-download underlying files.
#'
#' @return An `obr_tbl` with columns `forecast_date`, `period` (fiscal year
#' being forecast), `period_type`, `series`, `unit`, `value_forecast`
#' (from HFD), `value_actual` (from PFD outturn), and `error`
#' (`value_forecast - value_actual`). Provenance points at the HFD;
#' the PFD source URL is recorded in `notes`.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' eval <- obr_actual_vs_forecast("PSNB")
#'
#' # 1-year-ahead forecast errors only:
#' # take the forecast vintage closest to the start of each fiscal year
#' eval2425 <- eval[eval$period == "2024-25", ]
#' eval2425[order(eval2425$forecast_date), ]
#' options(op)
#' }
#'
#' @family forecasts
#' @export
obr_actual_vs_forecast <- function(series = c("PSNB", "PSND", "expenditure"),
                                   refresh = FALSE) {
  series <- match.arg(series)

  fn <- .actuals_fn_for(series)
  if (is.null(fn)) {
    cli::cli_abort(c(
      "No outturn function in this package for series {.val {series}}.",
      "i" = "Supported: {.val PSNB}, {.val PSND}, {.val expenditure}."
    ))
  }

  fc      <- get_forecasts(series, refresh = refresh)
  actuals <- fn(refresh = refresh)

  prov_fc  <- obr_provenance(fc)
  prov_act <- obr_provenance(actuals)

  fc_df  <- as.data.frame(fc)[,  c("forecast_date", "period", "period_type",
                                   "series", "unit", "value")]
  act_df <- as.data.frame(actuals)[, c("period", "value")]

  out <- merge(fc_df, act_df, by = "period",
               suffixes = c("_forecast", "_actual"))
  out$error <- out$value_forecast - out$value_actual
  out <- out[, c("forecast_date", "period", "period_type",
                 "series", "unit",
                 "value_forecast", "value_actual", "error")]
  rownames(out) <- NULL

  new_obr_tbl(
    data        = out,
    publication = prov_fc$publication,
    vintage     = prov_fc$vintage,
    source_url  = prov_fc$source_url,
    retrieved   = prov_fc$retrieved,
    file_md5    = prov_fc$file_md5,
    notes       = sprintf(
      paste0("Forecast vs outturn for %s. ",
             "error = value_forecast - value_actual. ",
             "Outturn source: %s"),
      series, prov_act$source_url
    )
  )
}
