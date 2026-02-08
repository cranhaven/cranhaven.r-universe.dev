#' Plot Brazilian exchange rate (USD/BRL)
#'
#' Generates a time series plot of the USD/BRL exchange rate using data from `get_exchange_rate()`.
#' Shows the commercial exchange rate for US Dollar to Brazilian Real.
#'
#' @param data Tibble returned by `get_exchange_rate()`
#' @param language Language for titles and labels: "pt" (Portuguese) or "eng" (English).
#'
#' @return A `ggplot2` object showing the exchange rate over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: English version
#' exchange_data <- get_exchange_rate("2023-01-01", "2023-12-31")
#' exchange_plot <- plot_exchange_rate(exchange_data)
#' print(exchange_plot)
#'
#' # Example 2: Portuguese version
#' dados_cambio <- get_exchange_rate("2023-01-01", "2023-12-31", language = "pt")
#' grafico_cambio <- plot_exchange_rate(dados_cambio, language = "pt")
#' print(grafico_cambio)
#' }

plot_exchange_rate <- function(data,
                               language = "eng") {

  # === PARAMETER VALIDATION ===

  # Validate 'data' parameter
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble", call. = FALSE)
  }

  required_cols <- c("date", "value")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "'data' must contain columns: ",
        paste(required_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.character(language) || length(language) != 1) {
    stop("'language' must be a single character string ('eng' or 'pt')", call. = FALSE)
  }

  language <- tolower(language)
  if (!language %in% c("eng", "pt")) {
    stop("'language' must be either 'eng' (English) or 'pt' (Portuguese)", call. = FALSE)
  }

  # === FUNCTION BODY ===

  # Declare global variables for dplyr operations
  value <- cotacao <- rate <- NULL

  # === TEXT DEFINITIONS ===

  if (language == "eng") {
    title   <- "Brazil | Exchange Rate (USD/BRL)"
    y_label <- "Exchange Rate (R$/US$)"
    caption <- "Source: Central Bank of Brazil"
    date_breaks <- "3 months"
  } else {
    title   <- "Brasil | Taxa de Cambio (USD/BRL)"
    y_label <- "Taxa de cambio (R$/US$)"
    caption <- "Fonte: Banco Central do Brasil"
    date_breaks <- "3 months"
  }

  # === PLOT ===

  .plot_time_series(
    data = data,
    x_var = "date",
    y_var = "value",
    plot_type = "line",
    title = title,
    y_label = y_label,
    caption = caption,
    y_suffix = NULL,
    color = "#2c3e50",
    show_points = TRUE,
    date_breaks = date_breaks
  )
}
