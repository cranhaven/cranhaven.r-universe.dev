#' Get IPCA Inflation Data
#'
#' Downloads monthly IPCA (Broad National Consumer Price Index) inflation data
#' from the Brazilian Central Bank's SGS API and calculates accumulated inflation rates.
#' IPCA is Brazil's official inflation index, calculated monthly by IBGE.
#'
#' @param start_date Start date for the data period. Accepts multiple formats:
#'   - `"YYYY"` for year only (e.g., `"2020"` becomes `"2020-01-01"`)
#'   - `"YYYY-MM"` for year and month (e.g., `"2020-06"` becomes `"2020-06-01"`)
#'   - `"YYYY-MM-DD"` for a specific date (e.g., `"2020-06-15"`)
#'   - `NULL` defaults to `"2020-01-01"` (aligned with other functions in the package)
#' @param end_date End date for the data period. Accepts the same formats as `start_date`:
#'   - `"YYYY"` (e.g., `"2023"` becomes `"2023-12-31"`)
#'   - `"YYYY-MM"` (e.g., `"2023-12"` becomes the last day of December 2023)
#'   - `"YYYY-MM-DD"` for a specific date
#'   - `NULL` defaults to the current date (today)
#' @param language Language for column names in the returned data.frame:
#'   - `"eng"` (default): Returns columns `date`, `monthly_inflation`, `ytd_inflation`, `twelve_month_inflation`
#'   - `"pt"`: Returns columns `data_referencia`, `inflacao_mensal`, `inflacao_acumulada_ano`, `inflacao_12_meses`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with inflation metrics. Columns depend on the `language` parameter:
#'   - English (`language = "eng"`):
#'     - `date` (Date): Reference month
#'     - `monthly_inflation` (numeric): Monthly IPCA variation (%)
#'     - `ytd_inflation` (numeric): Year-to-date accumulated inflation (%)
#'     - `twelve_month_inflation` (numeric): 12-month accumulated inflation (%)
#'   - Portuguese (`language = "pt"`):
#'     - `data_referencia` (Date): Mes de referencia
#'     - `inflacao_mensal` (numeric): Variacao mensal do IPCA (%)
#'     - `inflacao_acumulada_ano` (numeric): Inflacao acumulada no ano (%)
#'     - `inflacao_12_meses` (numeric): Inflacao acumulada nos ultimos 12 meses (%)
#'
#' @note
#' **Default Period**: When `start_date = NULL`, defaults to `"2020-01-01"`, providing
#' data from the start of 2020. This period covers significant economic events including
#' the COVID-19 pandemic and recent inflationary pressures in Brazil.
#'
#' **Data Processing**: This function automatically downloads an extra 12 months
#' of historical data to calculate 12-month accumulated inflation correctly.
#' The final output is filtered to show only the requested period.
#'
#' **Calculation Details**:
#' - Year-to-date inflation: Cumulative product of monthly rates within each calendar year
#' - 12-month inflation: Rolling 12-month cumulative product of monthly rates
#'
#' @examples
#' \dontrun{
#'   # Default: from 2020 to current date (aligned with SELIC function)
#'   df <- get_inflation_rate()
#'
#'   # Specific period with year-only format
#'   df2 <- get_inflation_rate("2021", "2023")
#'
#'   # Using year-month format for precise month selection
#'   df3 <- get_inflation_rate("2022-03", "2023-06")
#'
#'   # Portuguese column names and labels
#'   df4 <- get_inflation_rate(language = "pt")
#'
#'   # Without variable labels
#'   df5 <- get_inflation_rate("2020-01-01", "2022-12-31", labels = FALSE)
#'
#'   # Current year analysis
#'   current_year <- format(Sys.Date(), "%Y")
#'   df6 <- get_inflation_rate(start_date = current_year)
#'
#'   # Compare with SELIC rate (same default period)
#'   selic_data <- get_selic_rate()  # Also starts at 2020-01-01
#'   inflation_data <- get_inflation_rate()  # Same start date
#' }
#'
#' @export
get_inflation_rate <- function(start_date = "2012-01-01",
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
  value <- monthly_inflation <- ytd_inflation <- twelve_month_inflation <- year <- NULL

  # === DATE NORMALIZATION ===
  start_date_norm <- .normalize_date(start_date, is_start = TRUE)
  end_date_norm   <- .normalize_date(end_date, is_start = FALSE)

  download_start <- start_date_norm - lubridate::period(months = 12)

  # === DOWNLOAD DATA (IPCA mensal SGS 433) ===
  data <- .get_sgs_series(
    series_id = 433,
    start_date = format(download_start, "%Y-%m-%d"),
    end_date   = end_date
  )

  # === PROCESS DATA ===
  data <- data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      value = as.numeric(value),
      year  = lubridate::year(date)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      ytd_inflation = (cumprod(1 + value / 100) - 1) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      twelve_month_inflation = sapply(
        seq_along(value),
        function(i) {
          if (i < 12) return(NA_real_)
          (prod(1 + value[(i - 11):i] / 100) - 1) * 100
        }
      )
    ) |>
    dplyr::filter(date >= start_date_norm & date <= end_date_norm) |>
    dplyr::select(date, value, ytd_inflation, twelve_month_inflation)

  # === LABELS ===
  if (isTRUE(labels) && requireNamespace("labelled", quietly = TRUE)) {
    if (language == "pt") {
      data <- labelled::set_variable_labels(
        data,
        date  = "Mes de referencia",
        value = "Inflacao mensal IPCA (%)",
        ytd_inflation = "Inflacao acumulada no ano (%)",
        twelve_month_inflation = "Inflacao acumulada em 12 meses (%)"
      )
    } else {
      data <- labelled::set_variable_labels(
        data,
        date  = "Reference month",
        value = "Monthly IPCA inflation (%)",
        ytd_inflation = "Year-to-date inflation (%)",
        twelve_month_inflation = "12-month inflation (%)"
      )
    }
  }

  return(data)
}
