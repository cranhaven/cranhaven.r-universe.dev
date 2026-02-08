#' Compare multiple financial/time series indices
#'
#' Plots multiple time series on the same chart for comparison.
#'
#' @param data_list Named list of data frames, each returned by a get_* function
#' @param y_vars Vector of column names containing the values to plot from each data frame
#' @param date_vars Vector of column names containing dates from each data frame
#' @param language Language for labels: "pt" (Portuguese) or "eng" (English)
#' @param scale_type Type of scaling: "none" (raw values), "index" (index to 100),
#'                   "percent_change" (percentage change from first observation)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param y_label Y-axis label
#' @param caption Plot caption
#' @param colors Vector of colors for each series
#' @param line_types Vector of line types for each series
#' @param show_legend Whether to show the legend (default: TRUE)
#' @param legend_position Position of legend ("bottom", "top", "left", "right", or "none")
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' # Example comparing multiple series
#' selic <- get_selic_rate(2020, 2024)
#' ipca <- get_ipca(2020, 2024)
#' igpm <- get_igpm(2020, 2024)
#'
#' comparison_plot <- plot_series_comparison(
#'   data_list = list(SELIC = selic, IPCA = ipca, IGP-M = igpm),
#'   y_vars = c("rate", "value", "value"),
#'   date_vars = c("date", "date", "date"),
#'   scale_type = "index",
#'   title = "Comparison of Brazilian Economic Indicators",
#'   y_label = "Index (2020-01 = 100)",
#'   language = "eng"
#' )
#' print(comparison_plot)
#' }

plot_series_comparison <- function(data_list,
                                   y_vars,
                                   date_vars,
                                   language = "eng",
                                   scale_type = c("none", "index", "percent_change"),
                                   title = NULL,
                                   subtitle = NULL,
                                   y_label = NULL,
                                   caption = NULL,
                                   colors = NULL,
                                   line_types = NULL,
                                   show_legend = TRUE,
                                   legend_position = "bottom") {

  # === PARAMETER VALIDATION ===
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required. Install it with install.packages('ggplot2').")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Install it with install.packages('dplyr').")
  }

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("The 'tidyr' package is required. Install it with install.packages('tidyr').")
  }

  # === FUNCTION BODY ===
  # Declare global variables for dplyr operations
  value <- series <- NULL

  scale_type <- match.arg(scale_type)

  # Validate inputs
  n_series <- length(data_list)
  if (length(y_vars) != n_series || length(date_vars) != n_series) {
    stop("Length of data_list, y_vars, and date_vars must be the same")
  }

  if (is.null(names(data_list))) {
    names(data_list) <- paste0("Series_", seq_len(n_series))
  }

  # Prepare each series
  prepared_series <- list()

  for (i in seq_len(n_series)) {
    df <- data_list[[i]]
    series_name <- names(data_list)[i]

    # Select and rename columns
    df_prep <- df |>
      dplyr::select(
        date = !!dplyr::sym(date_vars[i]),
        value = !!dplyr::sym(y_vars[i])
      ) |>
      dplyr::mutate(series = series_name)

    # Apply scaling if requested
    if (scale_type == "index") {
      df_prep <- df_prep |>
        dplyr::mutate(value = 100 * value / value[1])
    } else if (scale_type == "percent_change") {
      df_prep <- df_prep |>
        dplyr::mutate(value = 100 * (value / value[1] - 1))
    }

    prepared_series[[i]] <- df_prep
  }

  # Combine all series
  combined_data <- dplyr::bind_rows(prepared_series)

  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00",
                "#6a3d9a", "#b15928", "#a6cee3", "#fb9a99")[seq_len(n_series)]
  }

  if (is.null(line_types)) {
    line_types <- rep("solid", n_series)
  }

  # Create plot
  p <- ggplot2::ggplot(combined_data,
                       ggplot2::aes(x = date, y = value,
                                    color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_linetype_manual(values = line_types) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = if (show_legend) legend_position else "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # Format x-axis if it's a date
  if (inherits(combined_data$date, c("Date", "POSIXct", "POSIXt"))) {
    p <- p + ggplot2::scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b/%Y"
    )
  }

  # Add suffix based on scale type
  if (scale_type == "index") {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number())
    if (is.null(y_label)) y_label <- "Index"
  } else if (scale_type == "percent_change") {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%"))
    if (is.null(y_label)) y_label <- "Percent Change"
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::label_number())
  }

  # Set default title if not provided
  if (is.null(title)) {
    if (language == "eng") {
      title <- "Comparison of Economic Indicators"
    } else {
      title <- "Comparacao de Indicadores Economicos"
    }
  }

  # Add labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = NULL,
    y = y_label,
    caption = caption,
    color = "Indicator",
    linetype = "Indicator"
  )

  return(p)
}
