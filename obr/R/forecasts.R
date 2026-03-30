# Stable OBR download URL for Historical Official Forecasts Database
FORECASTS_URL      <- "https://obr.uk/download/historical-official-forecasts-database-march-2025/"
FORECASTS_FILENAME <- "historical_forecasts.xlsx"

forecasts_path <- function(refresh = FALSE) {
  obr_fetch(FORECASTS_URL, FORECASTS_FILENAME, refresh = refresh)
}

# Map user-friendly series names to Excel sheet names
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

#' List available forecast series
#'
#' Returns a data frame showing the series names accepted by
#' \code{\link{get_forecasts}}, the corresponding Excel sheet in the OBR
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
#' and for comparing forecasts against outturns.
#'
#' @param series Character. The series to return. Use
#'   \code{\link{list_forecast_series}} to see all options. Defaults to
#'   `"PSNB"` (Public Sector Net Borrowing in £bn).
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy exists.
#'   Defaults to `FALSE`.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{series}{Series name as supplied (character)}
#'   \item{forecast_date}{When the forecast was published, e.g. `"March 2024"` (character)}
#'   \item{fiscal_year}{The fiscal year being forecast, e.g. `"2024-25"` (character)}
#'   \item{value}{Forecast value (numeric)}
#' }
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#' # All PSNB forecasts
#' get_forecasts("PSNB")
#'
#' # What did OBR forecast for 2024-25 PSNB at each Budget?
#' psnb <- get_forecasts("PSNB")
#' psnb[psnb$fiscal_year == "2024-25", ]
#' options(op)
#' }
#'
#' @family forecasts
#' @export
get_forecasts <- function(series = "PSNB", refresh = FALSE) {

  sheet_name <- SERIES_MAP[series]
  if (is.na(sheet_name)) {
    cli::cli_abort(c(
      "Unknown series {.val {series}}.",
      "i" = "Run {.fn list_forecast_series} to see available options."
    ))
  }

  path <- forecasts_path(refresh)
  raw  <- readxl::read_excel(path, sheet = sheet_name,
                              col_names = FALSE, .name_repair = "minimal")

  # Row 4: "Back to contents" | fiscal year column headers
  # Rows 5+: forecast date | values
  fiscal_years   <- as.character(unlist(raw[4, 2:ncol(raw)]))
  forecast_dates <- as.character(unlist(raw[5:nrow(raw), 1]))
  data_matrix    <- as.data.frame(raw[5:nrow(raw), 2:ncol(raw)],
                                  stringsAsFactors = FALSE)

  # Keep only rows where the date looks like "Month Year" (e.g. "March 2024")
  valid <- grepl("^[A-Za-z]+ [0-9]{4}", forecast_dates)
  forecast_dates <- forecast_dates[valid]
  data_matrix    <- data_matrix[valid, ]

  n_dates <- length(forecast_dates)
  n_years <- length(fiscal_years)

  result <- data.frame(
    series        = series,
    forecast_date = rep(forecast_dates, times  = n_years),
    fiscal_year   = rep(fiscal_years,   each   = n_dates),
    value         = suppressWarnings(as.numeric(unlist(data_matrix))),
    stringsAsFactors = FALSE
  )

  result[!is.na(result$value), ]
}
