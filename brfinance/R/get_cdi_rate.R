#' Get CDI Rate (Interbank Deposit Certificate)
#'
#' Downloads daily CDI (Certificado de Depósito Interbancário) rate from BCB/SGS.
#' This function retrieves the daily CDI rate (SGS series 12), which is the benchmark
#' interest rate for interbank transactions in Brazil.
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
#'   - `"eng"` (default): Returns columns `date` and `cdi_rate`
#'   - `"pt"`: Returns columns `data_referencia` and `taxa_cdi`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{date}{Reference date}
#'   \item{value}{Daily CDI rate (% per day)}
#'   \item{value_annualized}{Annualized CDI rate (% per year, 252 business days)}
#' }
#'
#' @note
#' **Series information**: This function uses SGS series 12, which represents the
#' daily CDI rate (taxa de juros - CDI). The CDI is a key benchmark for fixed income
#' investments and interbank lending in Brazil. Data is available from 1986 onward
#' with daily frequency (business days only).
#'
#' @examples
#' \dontrun{
#'   # Default: last 30 days of CDI rate
#'   df <- get_cdi_rate()
#'
#'   # Specific period
#'   df2 <- get_cdi_rate("2023-01-01", "2023-03-31")
#'
#'   # Using year-month format for a specific month
#'   df3 <- get_cdi_rate("2023-06", "2023-06")
#'
#'   # Portuguese column names and labels
#'   df4 <- get_cdi_rate(language = "pt")
#'
#'   # Complete example with all parameters
#'   df5 <- get_cdi_rate("2023-01-01", "2023-12-31", language = "pt", labels = TRUE)
#'
#'   # Historical analysis
#'   df6 <- get_cdi_rate("2020-03-01", "2020-04-30")  # COVID period
#' }
#'
#' @export
get_cdi_rate <- function(start_date = NULL,
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
  # Declare global variables for dplyr operations
  value <- cdi_rate <- value_annualized <- NULL

  # Use internal function to download data (SGS series 12 = CDI rate)
  data <- .get_sgs_series(
    series_id = 12,  # Código do CDI
    start_date = start_date,
    end_date = end_date
  )

  # Process the data
  data <- data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      value_annualized = ((1+value/100)^252-1)*100
    ) |>
    dplyr::select(
      date,
      value,
      value_annualized
    )

  # === VARIABLE LABELS ===
  if (isTRUE(labels) && requireNamespace("labelled", quietly = TRUE)) {

    if (language == "pt") {
      data <- labelled::set_variable_labels(
        data,
        date = "Data de referencia",
        value = "Taxa CDI diaria (% ao dia)",
        value_annualized = "Taxa CDI anualizada (% ao ano, 252 dias uteis)"
      )
    } else {
      data <- labelled::set_variable_labels(
        data,
        date = "Reference date",
        value = "Daily CDI rate (% per day)",
        value_annualized = "Annualized CDI rate (% per year, 252 business days)"
      )
    }
  }

  return(data)
}
