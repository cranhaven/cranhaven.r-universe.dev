#' Plot Brazilian Inflation Rate (IPCA)
#'
#' Generates a time series plot of Brazil's monthly inflation rate measured
#' by the IPCA (Índice Nacional de Preços ao Consumidor Amplo).
#' The IPCA is Brazil's official inflation index and is widely used for
#' monetary policy decisions, contract indexation, and economic analysis.
#'
#' @param data Tibble returned by `get_inflation_rate()`, with columns `date` and `value`.
#'   The `value` column represents the monthly IPCA inflation rate (%).
#' @param language Language for titles and labels: "pt" (Portuguese) or "eng" (English).
#'
#' @return A `ggplot2` object showing the monthly inflation rate over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: English version
#' inflation_data <- get_inflation_rate(2020, 2024)
#' inflation_plot <- plot_inflation_rate(inflation_data)
#' print(inflation_plot)
#'
#' # Example 2: Portuguese version
#' dados_inflacao <- get_inflation_rate(2020, 2024, language = "pt")
#' grafico_inflacao <- plot_inflation_rate(dados_inflacao, language = "pt")
#' print(grafico_inflacao)
#' }

plot_inflation_rate <- function(data, language = "eng") {

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

  # === TEXT DEFINITIONS ===
  if (language == "eng") {
    title   <- "Brazil | Inflation Rate (IPCA)"
    y_label <- "Monthly inflation (%)"
    caption <- "Source: IBGE / Central Bank of Brazil (SGS 433)"
  } else {
    title   <- "Brasil | Inflacao (IPCA)"
    y_label <- "Inflacao mensal (%)"
    caption <- "Fonte: IBGE / Banco Central do Brasil (SGS 433)"
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
    color = "#c0392b",
    show_points = TRUE,
    date_breaks = "6 months"
  )
}
