# Historical Official Forecasts Database.
# OBR sometimes re-uses an older slug across vintages, so the package tries
# recent fiscal events first, then falls back to the known-stable slug.
FORECASTS_FALLBACK <- "https://obr.uk/download/historical-official-forecasts-database-march-2025/"
FORECASTS_FILENAME <- "historical_forecasts.xlsx"

forecasts_source <- function(refresh = FALSE) {
  obr_get_xlsx(
    candidates = forecasts_url_candidates(),
    fallback   = FORECASTS_FALLBACK,
    filename   = FORECASTS_FILENAME,
    refresh    = refresh,
    label      = "Historical Forecasts Database"
  )
}

# User-friendly series name -> primary Excel sheet name.
# The literal Pound symbol in some OBR sheet names is fragile; the tolerant
# resolver in resolve_sheet_name() retries with a regex if the primary name
# is missing.
SERIES_MAP <- c(
  "PSNB"            = "\u00a3PSNB",
  "PSNB_pct"        = "PSNB",
  "PSND"            = "PSND",
  "receipts"        = "\u00a3PSCR",
  "receipts_pct"    = "PSCR",
  "expenditure"     = "\u00a3TME",
  "expenditure_pct" = "TME",
  "GDP"             = "NGDP",
  "real_GDP"        = "UKGDP",
  "CPI"             = "CPI"
)

# Regex fallbacks for series whose primary sheet name uses a non-ASCII
# currency symbol (Pound). For ASCII-only sheet names, the primary lookup
# is enough and no fallback is needed.
SERIES_FALLBACK <- c(
  "PSNB"        = "^.PSNB$",
  "receipts"    = "^.PSCR$",
  "expenditure" = "^.TME$"
)

# v0.4.0 schema metadata for each series in SERIES_MAP. Used to tag every
# returned row with metric_type and unit so callers can rbind() across series
# and know what the value column means.
SERIES_META <- list(
  "PSNB"            = list(metric_type = "level",   unit = "gbp_bn"),
  "PSNB_pct"        = list(metric_type = "pct",     unit = "pct"),
  "PSND"            = list(metric_type = "pct",     unit = "pct"),
  "receipts"        = list(metric_type = "level",   unit = "gbp_bn"),
  "receipts_pct"    = list(metric_type = "pct",     unit = "pct"),
  "expenditure"     = list(metric_type = "level",   unit = "gbp_bn"),
  "expenditure_pct" = list(metric_type = "pct",     unit = "pct"),
  "GDP"             = list(metric_type = "yoy_pct", unit = "pct"),
  "real_GDP"        = list(metric_type = "yoy_pct", unit = "pct"),
  "CPI"             = list(metric_type = "yoy_pct", unit = "pct")
)

#' List available forecast series
#'
#' Returns a data frame showing the series names accepted by
#' [get_forecasts()], the corresponding Excel sheet in the OBR
#' Historical Official Forecasts Database, and a plain-English description.
#'
#' @return A data frame with columns `series`, `sheet`, and `description`.
#'
#' @examples
#' list_forecast_series()
#'
#' @family forecasts
#' @export
list_forecast_series <- function() {
  data.frame(
    series = names(SERIES_MAP),
    sheet  = unname(SERIES_MAP),
    description = c(
      "Public sector net borrowing (\u00a3bn)",
      "Public sector net borrowing (% of GDP)",
      "Public sector net debt (% of GDP)",
      "Public sector current receipts (\u00a3bn)",
      "Public sector current receipts (% of GDP)",
      "Total managed expenditure (\u00a3bn)",
      "Total managed expenditure (% of GDP)",
      "Nominal GDP growth (%)",
      "Real GDP growth (%)",
      "CPI inflation (%)"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get OBR forecast history for a fiscal series
#'
#' Downloads (and caches) the OBR Historical Official Forecasts Database and
#' returns a tidy long-format data frame showing every forecast the OBR has
#' ever published for a given series. Each row is one forecast for one fiscal
#' year, made at one fiscal event.
#'
#' This is useful for visualising how OBR forecasts have evolved over time,
#' and for comparing forecasts against outturns. The vintage of the underlying
#' database is recorded in the returned object's provenance.
#'
#' @param series Character. The series to return. Use
#'   [list_forecast_series()] to see all options. Defaults to
#'   `"PSNB"` (Public Sector Net Borrowing in \enc{£}{GBP} billion).
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema plus a
#' `forecast_date` column (so each row identifies one fiscal-year value at
#' one fiscal event):
#' \describe{
#'   \item{forecast_date}{When the forecast was published, e.g. `"March 2024"`}
#'   \item{period}{Fiscal year being forecast, e.g. `"2024-25"`}
#'   \item{period_type}{Always `"fiscal_year"`}
#'   \item{series}{Series name as supplied (character)}
#'   \item{metric_type}{One of `"level"`, `"pct"`, `"yoy_pct"`, set per
#'     series. CPI / GDP / real_GDP are `"yoy_pct"` (rates of change);
#'     PSNB / receipts / expenditure are `"level"`; the `_pct` variants and
#'     PSND are `"pct"` (% of GDP).}
#'   \item{value}{Numeric forecast value in the unit described by `unit`}
#'   \item{unit}{`"gbp_bn"` for level series, `"pct"` for percentage series}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' get_forecasts("PSNB")
#'
#' psnb <- get_forecasts("PSNB")
#' psnb[psnb$period == "2024-25", ]
#' options(op)
#' }
#'
#' @family forecasts
#' @export
get_forecasts <- function(series = "PSNB", refresh = FALSE) {

  series <- match.arg(series, choices = names(SERIES_MAP))

  src <- forecasts_source(refresh)
  primary  <- SERIES_MAP[[series]]
  fallback <- if (series %in% names(SERIES_FALLBACK)) {
    SERIES_FALLBACK[[series]]
  } else {
    NULL
  }
  sheet_name <- resolve_sheet_name(src$path, primary, fallback)

  raw  <- readxl::read_excel(src$path, sheet = sheet_name,
                              col_names = FALSE, .name_repair = "minimal")

  fiscal_years   <- as.character(unlist(raw[4, 2:ncol(raw)]))
  forecast_dates <- as.character(unlist(raw[5:nrow(raw), 1]))
  data_matrix    <- as.data.frame(raw[5:nrow(raw), 2:ncol(raw)],
                                  stringsAsFactors = FALSE)

  valid <- grepl("^[A-Za-z]+ [0-9]{4}", forecast_dates)
  forecast_dates <- forecast_dates[valid]
  data_matrix    <- data_matrix[valid, ]

  n_dates <- length(forecast_dates)
  n_years <- length(fiscal_years)

  meta <- SERIES_META[[series]]

  result <- data.frame(
    forecast_date = rep(forecast_dates, times  = n_years),
    period        = rep(fiscal_years,   each   = n_dates),
    period_type   = "fiscal_year",
    series        = series,
    metric_type   = meta$metric_type,
    value         = suppressWarnings(as.numeric(unlist(data_matrix))),
    unit          = meta$unit,
    stringsAsFactors = FALSE
  )

  result <- result[!is.na(result$value), ]
  rownames(result) <- NULL

  new_obr_tbl(
    data        = result,
    publication = "HFD",
    vintage     = obr_url_vintage(src$url),
    source_url  = src$url,
    retrieved   = src$retrieved,
    file_md5    = src$file_md5
  )
}
