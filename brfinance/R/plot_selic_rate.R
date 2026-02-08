#' Plot Brazilian SELIC rate (annualized, base 252)
#'
#' Generates a time series plot of the SELIC interest rate using data from `get_selic()`.
#' The SELIC rate ("Sistema Especial de Liquidação e de Custódia") represents the
#' effective annualized rate (252-business-day basis) for overnight interbank loans
#' and is the main instrument of Brazil’s monetary policy.
#'
#' @param data Tibble returned by `get_selic_rate()`
#' @param language Language for titles and labels: "pt" (Portuguese) or "eng" (English).
#'
#' @return A `ggplot2` object showing the SELIC rate over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: English version
#' selic_data <- get_selic_rate(2020, 2024)
#' selic_plot <- plot_selic_rate(selic_data)
#' print(selic_plot)
#'
#' # Example 2: Portuguese version
#' dados_selic <- get_selic_rate(2020, 2024, language = "pt")
#' grafico_selic <- plot_selic_rate(dados_selic, language = "pt")
#' print(grafico_selic)
#' }

plot_selic_rate <- function(data,
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
  value <- selic_rate <- data_referencia <- taxa_selic <- NULL

  # === TEXT DEFINITIONS ===

  if (language == "eng") {
    title   <- "Brazil | SELIC Interest Rate (Effective Annual, 252-day basis)"
    y_label <- "SELIC Rate (% p.a.)"
    caption <- "Source: Central Bank of Brazil (SGS 1178)"
  } else {
    title   <- "Brasil | Taxa SELIC (Efetiva Anualizada, base 252)"
    y_label <- "Taxa SELIC (% a.a.)"
    caption <- "Fonte: Banco Central do Brasil (SGS 1178)"
  }

  # === PLOT ===

  .plot_time_series(
    data = data,
    x_var = "date",
    y_var = "value",
    plot_type = "step",
    title = title,
    y_label = y_label,
    caption = caption,
    y_suffix = "%",
    color = "#1f78b4",
    show_points = TRUE
  )
}
