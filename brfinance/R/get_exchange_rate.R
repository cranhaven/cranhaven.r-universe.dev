#' Get US Dollar Exchange Rate (Commercial)
#'
#' Downloads daily US dollar exchange rate (commercial, selling) from BCB/SGS.
#' This function retrieves the daily exchange rate (SGS series 1) in Brazilian Real (R$).
#'
#' @param start_date Start date for the data period. Accepts multiple formats:
#'   - `"YYYY"` for year only (e.g., `"2020"` becomes `"2020-01-01"`)
#'   - `"YYYY-MM"` for year and month (e.g., `"2020-06"` becomes `"2020-06-01"`)
#'   - `"YYYY-MM-DD"` for a specific date (e.g., `"2020-06-15"`)
#' @param end_date End date for the data period. Accepts the same formats as `start_date`:
#'   - `"YYYY"` (e.g., `"2023"` becomes `"2023-12-31"`)
#'   - `"YYYY-MM"` (e.g., `"2023-12"` becomes the last day of December 2023)
#'   - `"YYYY-MM-DD"` for a specific date
#'   - `NULL` defaults to the current date (today)
#' @param language Language for column names in the returned data.frame:
#'   - `"eng"` (default): Returns columns `date` and `exchange_rate`
#'   - `"pt"`: Returns columns `data_referencia` and `taxa_cambio`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with US dollar exchange rate. Columns depend on the `language` parameter:
#'   - English (`language = "eng"`): `date` (Date), `exchange_rate` (numeric, R$/US$)
#'   - Portuguese (`language = "pt"`): `data_referencia` (Date), `taxa_cambio` (numeric, R$/US$)
#'
#' @note
#' **Series information**: This function uses SGS series 1, which represents the
#' commercial US dollar selling rate (taxa de câmbio livre - dólar americano - venda).
#' Data is available from 1984 onward with daily frequency.
#'
#' @examples
#' \dontrun{
#'   # Default: last 30 days of exchange rate
#'   df <- get_exchange_rate()
#'
#'   # Specific period
#'   df2 <- get_exchange_rate("2023-01-01", "2023-03-31")
#'
#'   # Using year-month format for a specific month
#'   df3 <- get_exchange_rate("2023-06", "2023-06")
#'
#'   # Portuguese column names and labels
#'   df4 <- get_exchange_rate(language = "pt")
#'
#'   # Complete example with all parameters
#'   df5 <- get_exchange_rate("2023-01-01", "2023-12-31", language = "pt", labels = TRUE)
#' }
#'
#' @export
get_exchange_rate <- function(start_date = NULL,
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

  # Set default start_date if NULL (last 30 days by default for CDI)
  if (is.null(start_date)) {
    start_date <- Sys.Date() - 30  # Last 30 days
  }

  # === FUNCTION BODY ===

  # Set default start_date if NULL (last 30 days by default for exchange rate)
  if (is.null(start_date)) {
    start_date <- Sys.Date() - 30  # Last 30 days
  }

  # === FUNCTION BODY ===
  # Declare global variables for dplyr operations
  value <- exchange_rate <- NULL

  data <- .get_sgs_series(
    series_id = 1,  # Código do dólar comercial (venda)
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
        value = "Taxa de cambio (R$/US$) - comercial, venda"
      )
    } else {
      data <- labelled::set_variable_labels(
        data,
        date = "Reference date",
        value = "Exchange rate (R$/US$) - commercial, selling"
      )
    }
  }

  return(data)
}
