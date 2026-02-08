#' Get Brazilian Quarterly Unemployment Rate
#'
#' Downloads quarterly unemployment rate data from IBGE's Continuous PNAD survey
#' via the SIDRA API (table 6381). The unemployment rate represents the percentage
#' of the labor force that is unemployed and actively seeking employment.
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
#'   - `"eng"` (default): Returns columns `quarter`, `rate`, `month`, `year`, `date`
#'   - `"pt"`: Returns columns `trimestre`, `taxa`, `mes`, `ano`, `data`
#' @param labels Logical indicating whether to add variable labels using the `labelled`
#'   package. Labels provide descriptive text for each column when available.
#'
#' @return A data.frame with unemployment rate. Columns depend on the `language` parameter:
#'   - English (`language = "eng"`):
#'     - `quarter` (character): Moving quarter reference (e.g., "jan-mar 2023")
#'     - `rate` (numeric): Unemployment rate (%)
#'     - `month` (character): Quarter ending month abbreviation
#'     - `year` (numeric): Year
#'     - `date` (Date): Reference date (first day of quarter ending month)
#'   - Portuguese (`language = "pt"`):
#'     - `trimestre` (character): Trimestre móvel de referência
#'     - `taxa` (numeric): Taxa de desemprego (%)
#'     - `mes` (character): Mês de término do trimestre
#'     - `ano` (numeric): Ano
#'     - `data` (Date): Data de referência (primeiro dia do mês de término)
#'
#' @note
#' **Data Source**: IBGE's Continuous National Household Sample Survey (PNAD Contínua),
#' table 6381 (Unemployment rate), series 4099. Data is available from 2012 onward
#' with quarterly frequency (moving quarters).
#'
#' **Date Calculation**: The `date` column represents the first day of the month
#' that ends the moving quarter (e.g., "jan-mar 2023" becomes "2023-03-01").
#'
#' @examples
#' \dontrun{
#'   # Default: from 2020 to current date (aligned with other functions)
#'   df <- get_unemployment()
#'
#'   # Specific period with year-only format
#'   df2 <- get_unemployment("2018", "2023")
#'
#'   # Portuguese column names and labels
#'   df3 <- get_unemployment(language = "pt")
#'
#'   # Without variable labels
#'   df4 <- get_unemployment("2020-01-01", "2022-12-31", labels = FALSE)
#'
#'   # Compare unemployment with inflation (same period)
#'   unemployment_data <- get_unemployment("2020", "2023")
#'   inflation_data <- get_inflation_rate("2020", "2023")
#' }
#'
#' @export
get_unemployment <- function(start_date = "2020-01-01",
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
  value <- trimestre <- month <- year <- data <- NULL

  # === NORMALIZE DATES ===
  # Convert start_date and end_date to Date objects for filtering
  start_date_norm <- .normalize_date(start_date, is_start = TRUE)
  end_date_norm <- .normalize_date(end_date, is_start = FALSE)

  # Extract years for SIDRA API filtering
  start_year <- lubridate::year(start_date_norm)
  end_year <- lubridate::year(end_date_norm)

  # === DOWNLOAD AND PROCESS DATA ===
  # Download data from SIDRA API (table 6381, series 4099)
  dados <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

  # Process data (MANTENDO TODOS OS NOMES DE VARIÁVEIS ORIGINAIS)
  df <- dados |>
    janitor::clean_names() |>
    dplyr::select("trimestre_movel", "valor") |>
    dplyr::rename(trimestre = "trimestre_movel", value = "valor") |>
    dplyr::mutate(
      month = stringr::str_extract(trimestre, "(jan|fev|mar|abr|mai|jun|jul|ago|set|out|nov|dez)(?=\\s)"),
      year = as.numeric(stringr::str_extract(trimestre, "\\d{4}$")),
      date = lubridate::dmy(paste("01", month, year))
    ) |>
    dplyr::filter(year >= start_year & year <= end_year)

  if (language == "eng") {
    colnames(df) <- c("quarter", "value", "month", "year", "date")
  } else {
    colnames(df) <- c("trimestre", "value", "mes", "ano", "date")
  }

  # Additional filtering by exact date range (more precise than just year)
  df <- df |>
    dplyr::filter(date >= start_date_norm & date <= end_date_norm)

  # Add labels if requested
  if (isTRUE(labels) && requireNamespace("labelled", quietly = TRUE)) {
    if (language == "pt") {
      df <- labelled::set_variable_labels(
        df,
        trimestre = "Trimestre movel de referencia",
        value = "Taxa de desemprego (%)",
        mes = "Mes de termino do trimestre",
        ano = "Ano",
        date = "Data de referencia (primeiro dia do mes de termino)"
      )
    } else {
      df <- labelled::set_variable_labels(
        df,
        quarter = "Moving quarter reference",
        value = "Unemployment rate (%)",
        month = "Quarter ending month",
        year = "Year",
        date = "Reference date (first day of quarter ending month)"
      )
    }
  }

  return(df)
}
