#' Get GDP Growth Rate
#'
#' Downloads quarterly GDP growth data (% change) from BCB/SGS (Brazilian Central Bank).
#' This function retrieves nominal GDP values (SGS series 2010) and calculates the
#' quarter-over-quarter growth rate.
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
#'   - `"eng"` (default): Returns columns `date` and `gdp_growth`
#'   - `"pt"`: Returns columns `data_referencia` and `crescimento_pib`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with GDP growth rate. Columns depend on the `language` parameter:
#'   - English (`language = "eng"`): `date` (Date), `gdp_growth` (numeric, %)
#'   - Portuguese (`language = "pt"`): `data_referencia` (Date), `crescimento_pib` (numeric, %)
#'
#' @note
#' **Important limitation**: The nominal GDP series (SGS 2010) is currently
#' available only until 2014. Requests for periods after 2014 will return
#' empty results or a warning.
#'
#' @examples
#' \dontrun{
#'   # Default: data from 2000 to current date (but limited to 2014)
#'   df <- get_gdp_growth()
#'
#'   # Specific period (within available range)
#'   df2 <- get_gdp_growth("2010", "2014")
#'
#'   # Using year-month format
#'   df3 <- get_gdp_growth("2012-06", "2013-12")
#'
#'   # End date only (from earliest available to 2020-12-31)
#'   df4 <- get_gdp_growth(end_date = "2020-12-01")
#'
#'   # Portuguese column names and labels
#'   df5 <- get_gdp_growth(language = "pt")
#'
#'   # Complete example with all parameters
#'   df6 <- get_gdp_growth("2011-01-01", "2014-12-31", language = "pt", labels = TRUE)
#' }
#'
#' @export
get_gdp_growth <- function(start_date = "2000-01-01",
                           end_date = NULL,
                           language = "eng",
                           labels = TRUE) {

  # === PARAMETER VALIDATION ===
  if (!is.character(language) || length(language) != 1) {
    stop("'language' must be a single character string ('eng' or 'pt')", call. = FALSE)
  }

  language <- tolower(language)
  if (!language %in% c("eng", "pt")) {
    stop("'language' must be either 'eng' or 'pt'", call. = FALSE)
  }

  if (!is.logical(labels) || length(labels) != 1) {
    stop("'labels' must be a single logical value (TRUE or FALSE)", call. = FALSE)
  }

  # === FUNCTION BODY ===
  value <- NULL

  # Download nominal GDP series (SGS 2010)
  data <- .get_sgs_series(
    series_id = 2010,
    start_date = start_date,
    end_date = end_date
  )

  # Compute quarterly growth rate
  data <- data |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      value = (as.numeric(value) / dplyr::lag(as.numeric(value)) - 1) * 100
    ) |>
    dplyr::select(date, value)

  # Add labels if requested
  if (isTRUE(labels) && requireNamespace("labelled", quietly = TRUE)) {

    if (language == "pt") {
      data <- labelled::set_variable_labels(
        data,
        date  = "Trimestre de referencia",
        value = "Crescimento do PIB (%)"
      )
    } else {
      data <- labelled::set_variable_labels(
        data,
        date  = "Reference quarter",
        value = "GDP growth rate (%)"
      )
    }
  }

  return(data)
}
