#' Plot Brazil's quarterly unemployment rate
#'
#' Generates a ggplot2 line chart of unemployment rate in Brazil.
#'
#' @param data Tibble returned by `get_unemployment()`
#' @param language Language for column names: "pt" for Portuguese or "eng" (default) for English
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: English version
#' unemployment_data <- get_unemployment(2020, 2024)
#' unemployment_plot <- plot_unemployment(unemployment_data)
#' print(unemployment_plot)
#'
#' # Example 2: Portuguese version
#' dados_desemprego <- get_unemployment(2020, 2024, language = "pt")
#' grafico_desemprego <- plot_unemployment(dados_desemprego, language = "pt")
#' print(grafico_desemprego)
#' }

plot_unemployment <- function(data,
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

  # === TEXT DEFINITIONS ===

  if (language == "eng") {
    title   <- "Brazil | Unemployment Rate (Continuous PNAD)"
    y_label <- "Unemployment Rate (%)"
    caption <- "Source: IBGE - SIDRA (Table 6381)"
  } else {
    title   <- "Brasil | Taxa de Desemprego (PNAD Continua)"
    y_label <- "Taxa de desemprego (%)"
    caption <- "Fonte: IBGE - SIDRA (Tabela 6381)"
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
    color = "#2c3e50",
    show_points = TRUE,
    date_breaks = "1 year"
  )
}
