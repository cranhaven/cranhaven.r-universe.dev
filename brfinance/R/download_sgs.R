#' Download SGS series from Brazilian Central Bank
#'
#' Internal helper function to download time series data from BCB SGS API.
#' Uses httr2 for robust HTTP requests with automatic fallback strategy.
#'
#' @param series_id Numeric. SGS series ID.
#' @param start_date Start date (YYYY, YYYY-MM, or YYYY-MM-DD format).
#' @param end_date End date (YYYY, YYYY-MM, YYYY-MM-DD format, or NULL for current date).
#'
#' @return A data.frame with columns 'date' (Date) and 'value' (numeric).
#' @keywords internal
.get_sgs_series <- function(series_id,
                            start_date = NULL,
                            end_date = NULL) {

  # Declare global variables
  value <- data <- valor <- NULL

  # Normalize dates
  data_inicio <- .normalize_date(start_date, is_start = TRUE)
  data_fim   <- .normalize_date(end_date, is_start = FALSE)

  # Attempt download WITH date filters
  dados_baixados <- tryCatch({

    url_filtrado <- sprintf(
      'https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s&dataFinal=%s',
      series_id,
      format(data_inicio, '%d/%m/%Y'),
      format(data_fim, '%d/%m/%Y')
    )

    resposta <- url_filtrado |>
      httr2::request() |>
      httr2::req_perform()

    httr2::resp_body_json(resposta, simplifyVector = TRUE)

  }, error = function(e) {
    # Fallback: download WITHOUT date filters
    message(sprintf("Series %s: Filtered download failed. Fetching full series...", series_id))

    url_completo <- sprintf(
      'https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json',
      series_id
    )

    resposta <- url_completo |>
      httr2::request() |>
      httr2::req_perform()

    httr2::resp_body_json(resposta, simplifyVector = TRUE)
  })

  # Convert to data.frame (handle both list and data.frame returns)
  if (is.data.frame(dados_baixados)) {
    df <- dados_baixados
  } else {
    df <- dplyr::bind_rows(dados_baixados)
  }

  # Clean and standardize
  df <- df |>
    dplyr::mutate(
      date = as.Date(data, format = "%d/%m/%Y"),
      value = as.numeric(gsub(",", ".", valor, fixed = TRUE))
    ) |>
    dplyr::arrange(date) |>
    dplyr::select(date, value)  # Standard column names

  # Apply date filters locally (ensures precision)
  df <- dplyr::filter(df, date >= data_inicio & date <= data_fim)

  # Check if data exists for requested period
  if (nrow(df) == 0) {
    periodo_disponivel <- if (nrow(df) == 0) {
      "No data available"
    } else {
      paste(format(range(df$date), "%Y-%m"), collapse = " to ")
    }

    warning(sprintf(
      "Series %s has no data for requested period (%s to %s). Available: %s",
      series_id,
      format(data_inicio, "%Y-%m"),
      format(data_fim, "%Y-%m"),
      periodo_disponivel
    ), call. = FALSE)

    # Return empty data.frame with correct structure
    return(data.frame(date = as.Date(character()), value = numeric()))
  }

  return(df)
}

# NORMALIZAÇÃO DE DATAS

.normalize_date <- function(x, is_start = TRUE) {

  # Handle NULL: start = distant past, end = today
  if (is.null(x)) {
    return(if (is_start) as.Date("1900-01-01") else Sys.Date())
  }

  # Ensure character
  x <- as.character(x)

  # Year only: "2020"
  if (nchar(x) == 4 && grepl("^\\d{4}$", x)) {
    return(as.Date(paste0(x, if (is_start) "-01-01" else "-12-31")))
  }

  # Year-month: "2020-06"
  if (nchar(x) == 7 && grepl("^\\d{4}-\\d{2}$", x)) {
    if (is_start) {
      return(as.Date(paste0(x, "-01")))
    } else {
      # Simple approach: use day 28 (safe for all months in BCB API)
      return(as.Date(paste0(x, "-28")))
    }
  }

  # Full date: "2020-06-15" (or any other valid format)
  tryCatch(
    as.Date(x),
    error = function(e) {
      stop(
        sprintf("Invalid date format: '%s'. Use: NULL, YYYY, YYYY-MM, or YYYY-MM-DD", x),
        call. = FALSE
      )
    }
  )
}
