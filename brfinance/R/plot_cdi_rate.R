#' Plot Brazilian CDI rate
#'
#' Generates a time series plot of the CDI (Certificado de Depósito Interbancário) rate.
#' The CDI is the benchmark interest rate for interbank deposits in Brazil and serves
#' as a reference for many fixed income investments.
#'
#' @param data Tibble returned by `get_cdi_rate()`, with columns `date` and `value`.
#' @param language Language for titles and labels: "pt" (Portuguese) or "eng" (English).
#'
#' @return A `ggplot2` object showing the CDI rate over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: English version
#' cdi_data <- get_cdi_rate(2020, 2024)
#' cdi_plot <- plot_cdi_rate(cdi_data)
#' print(cdi_plot)
#'
#' # Example 2: Portuguese version
#' dados_cdi <- get_cdi_rate(2020, 2024, language = "pt")
#' grafico_cdi <- plot_cdi_rate(dados_cdi, language = "pt")
#' print(grafico_cdi)
#' }

plot_cdi_rate <- function(data,
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
  value <- taxa_cdi <- data_referencia <- rate <- NULL

  # === TEXT DEFINITIONS ===

  if (language == "eng") {
    title   <- "Brazil | CDI Rate"
    y_label <- "Daily CDI rate (% per day)"
    caption <- "Source: Central Bank of Brazil"
  } else {
    title   <- "Brasil | Taxa CDI"
    y_label <- "Taxa CDI diaria (% ao dia)"
    caption <- "Fonte: Banco Central do Brasil"
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
    y_suffix = "%",
    color = "#3498db",
    show_points = TRUE,
    date_breaks = "6 months"
  )
}
