# Convert the OBR Historical Forecasts Database from tidy long format
# (rows = forecast vintage x fiscal year) into a wide real-time panel
# (rows = forecast vintage, columns = fiscal year). Useful for plotting
# how successive OBR forecasts evolved for the same fiscal year, or for
# computing forecast errors against outturn.

# Convert an OBR-style forecast date string ("March 2024", "October 2010")
# into an integer index for chronological ordering. NA on unrecognised input.
forecast_date_to_index <- function(date_str) {
  parts <- strsplit(date_str, " ", fixed = TRUE)
  vapply(parts, function(p) {
    if (length(p) < 2L) return(NA_integer_)
    m <- match(tolower(p[1]), tolower(month.name))
    y <- suppressWarnings(as.integer(p[2]))
    if (is.na(m) || is.na(y)) return(NA_integer_)
    as.integer(y * 12L + m)
  }, integer(1))
}

#' Build a wide real-time panel of OBR forecasts
#'
#' Takes the long-format Historical Forecasts Database output of
#' [get_forecasts()] and pivots it to a wide panel with one row per
#' forecast vintage (e.g. `"March 2024"`) and one column per fiscal year
#' being forecast (e.g. `"2024-25"`). The first column is the forecast
#' vintage; remaining columns are numeric forecast values.
#'
#' This format mirrors how the OBR's own Historical Forecasts Database is
#' laid out and how forecast-error studies (e.g. the OBR's own Forecast
#' Evaluation Report) decompose performance: each row is one forecast
#' published at a fiscal event, each column is the target year being
#' forecast, and the diagonal-like structure lets you read the h-step
#' ahead forecast for any vintage.
#'
#' @param series Character. Series to return; see [list_forecast_series()].
#'   Defaults to `"PSNB"`.
#' @param refresh Logical. If `TRUE`, re-download the underlying database.
#' @param order_chronologically Logical. If `TRUE` (the default), rows are
#'   ordered from earliest to latest forecast vintage. If `FALSE`, rows
#'   appear in the order returned by [get_forecasts()].
#'
#' @return An `obr_tbl` whose first column (`forecast_date`) is character
#'   and whose remaining columns are numeric forecast values, one per
#'   fiscal year. Provenance is inherited from the underlying call to
#'   [get_forecasts()] and a `notes` field describes the panel.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' panel <- obr_forecast_panel("PSNB")
#' # PSNB forecast for 2024-25 across every vintage
#' panel[, c("forecast_date", "2024-25")]
#' options(op)
#' }
#'
#' @family forecasts
#' @export
obr_forecast_panel <- function(series = "PSNB",
                               refresh = FALSE,
                               order_chronologically = TRUE) {
  fc   <- get_forecasts(series = series, refresh = refresh)
  prov <- attr(fc, "obr_provenance")
  long <- as.data.frame(fc)

  if (order_chronologically) {
    long$.idx <- forecast_date_to_index(long$forecast_date)
    long <- long[order(long$.idx), , drop = FALSE]
    long$.idx <- NULL
  }

  # Use stats::reshape for the long-to-wide pivot.
  # `period` replaced `fiscal_year` in v0.4.0; the source column is still
  # the fiscal year being forecast.
  wide <- stats::reshape(
    long[, c("forecast_date", "period", "value")],
    idvar     = "forecast_date",
    timevar   = "period",
    direction = "wide"
  )
  rownames(wide) <- NULL
  names(wide) <- sub("^value\\.", "", names(wide))

  # Sort columns: forecast_date first, then fiscal years ascending
  yr_cols <- setdiff(names(wide), "forecast_date")
  yr_cols <- yr_cols[order(yr_cols)]
  wide    <- wide[, c("forecast_date", yr_cols), drop = FALSE]

  new_obr_tbl(
    data        = wide,
    publication = prov$publication,
    vintage     = prov$vintage,
    source_url  = prov$source_url,
    retrieved   = prov$retrieved,
    file_md5    = prov$file_md5,
    notes       = sprintf(
      "Wide real-time panel for %s: rows = forecast vintage, columns = fiscal year.",
      series
    )
  )
}
