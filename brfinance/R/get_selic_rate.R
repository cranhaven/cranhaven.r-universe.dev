#' Get Annual Brazilian SELIC Rate (Annualized, Base 252)
#'
#' Downloads the annual SELIC rate series from the Central Bank of Brazil's SGS API.
#' The SELIC rate (Special System for Settlement and Custody) is Brazil's benchmark
#' overnight interest rate, used as the primary monetary policy instrument.
#'
#' @param start_date Start date for the data period. Accepts multiple formats:
#'   - `"YYYY"` for year only (e.g., `"2020"` becomes `"2020-01-01"`)
#'   - `"YYYY-MM"` for year and month (e.g., `"2020-06"` becomes `"2020-06-01"`)
#'   - `"YYYY-MM-DD"` for a specific date (e.g., `"2020-06-15"`)
#'   - `NULL` defaults to `"2020-01-01"` (sensible default for analysis)
#' @param end_date End date for the data period. Accepts the same formats as `start_date`:
#'   - `"YYYY"` (e.g., `"2023"` becomes `"2023-12-31"`)
#'   - `"YYYY-MM"` (e.g., `"2023-12"` becomes the last day of December 2023)
#'   - `"YYYY-MM-DD"` for a specific date
#'   - `NULL` defaults to the current date (today)
#' @param language Language for column names in the returned data.frame:
#'   - `"eng"` (default): Returns columns `date` and `selic_rate`
#'   - `"pt"`: Returns columns `data_referencia` and `taxa_selic`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with SELIC rate. Columns depend on the `language` parameter:
#'   - English (`language = "eng"`): `date` (Date), `selic_rate` (numeric, % per year)
#'   - Portuguese (`language = "pt"`): `data_referencia` (Date), `taxa_selic` (numeric, % ao ano)
#'
#' @note
#' **IMPORTANT API LIMITATION**: The BCB API imposes a **10-year maximum window**
#' for daily frequency series like SELIC. Requests spanning more than 10 years will fail.
#' For longer historical analyses, split your request into multiple 10-year periods.
#'
#' **DEFAULT PERIOD**: When `start_date = NULL`, defaults to `"2020-01-01"` (start of 2020),
#' providing recent data while avoiding the 10-year API limit with current dates.
#'
#' @examples
#' \dontrun{
#'   # Default: from 2020 to current date
#'   df <- get_selic_rate()
#'
#'   # Specific period within 10-year limit
#'   df2 <- get_selic_rate("2020-01-01", "2023-12-31")
#'
#'   # Last 5 years (respecting 10-year limit)
#'   df3 <- get_selic_rate(start_date = "2019")
#'
#'   # Portuguese column names and labels
#'   df4 <- get_selic_rate(language = "pt")
#'
#'   # Complete year analysis
#'   df5 <- get_selic_rate("2018", "2023")
#' }
#'
#' @export
get_selic_rate <- function(start_date = "2020-01-01",
                           end_date = NULL,
                           language = "eng",
                           labels = TRUE) {

  # === PARAMETER VALIDATION ===
  # Validate 'language' parameter
  if (!is.character(language) || length(language) != 1) {
    stop("'language' must be a single character string ('eng' or 'pt')", call. = FALSE)
  }

  language <- tolower(language)
  if (!language %in% c("eng", "pt")) {
    stop("'language' must be either 'eng' (English) or 'pt' (Portuguese)", call. = FALSE)
  }

  # Validate 'labels' parameter
  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)", call. = FALSE)
  }

  # === FUNCTION BODY ===
  # Declare global variables for dplyr operations
  value <- selic_rate <- NULL

  # Use internal function to download data
  data <- .get_sgs_series(
    series_id = 1178,  # Código da SELIC anual
    start_date = start_date,
    end_date = end_date
  )

  # Process the data
  data <- data |>
    dplyr::arrange(date) |>
    dplyr::select(
      date,
      value
    )

  # === VARIABLE LABELS ===
  if (isTRUE(labels) && requireNamespace("labelled", quietly = TRUE)) {

    if (language == "pt") {
      data <- labelled::set_variable_labels(
        data,
        date = "Data de referencia",
        value = "Taxa SELIC anual (% ao ano)"
      )
    } else {
      data <- labelled::set_variable_labels(
        data,
        date = "Reference date",
        value = "Annual SELIC rate (% per year)"
      )
    }
  }

  return(data)
}
