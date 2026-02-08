#' Internal function to generate standardized time series plots
#'
#' This function provides a flexible way to create time series plots with
#' consistent styling across the brfinance package.
#'
#' @param data Data frame/tibble with time series data
#' @param x_var Name of the column to use for x-axis (date/time variable)
#' @param y_var Name of the column to use for y-axis (value variable)
#' @param plot_type Type of plot: "line" (default), "step", "bar", "point"
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param x_label Label for x-axis (NULL for no label)
#' @param y_label Label for y-axis
#' @param caption Plot caption
#' @param color Line/point color (default: "#1f78b4" for line, "#e31a1c" for points)
#' @param line_size Line thickness
#' @param point_size Point size (if plot_type includes points)
#' @param date_breaks Break interval for date axis (e.g., "6 months")
#' @param date_labels Format for date labels (e.g., "%b/%Y")
#' @param y_suffix Suffix for y-axis labels (e.g., "%", "R$")
#' @param theme_base_size Base font size for theme
#' @param rotate_x_angle Angle for x-axis text (default: 45)
#' @param show_points Whether to show points on line/step plots (default: TRUE)
#' @param ... Additional arguments passed to ggplot2 geoms
#'
#' @return A ggplot2 object
#' @keywords internal
#' @noRd

.plot_time_series <- function(data,
                              x_var,
                              y_var,
                              plot_type = c("line", "step", "bar", "point"),
                              title = NULL,
                              subtitle = NULL,
                              x_label = NULL,
                              y_label = NULL,
                              caption = NULL,
                              color = NULL,
                              line_size = 1,
                              point_size = 2,
                              date_breaks = "6 months",
                              date_labels = "%b/%Y",
                              y_suffix = NULL,
                              theme_base_size = 14,
                              rotate_x_angle = 45,
                              show_points = TRUE) {

  plot_type <- match.arg(plot_type)

  if (is.null(color)) {
    color <- if (plot_type == "step") "#1f78b4" else "#2c3e50"
  }

  point_color <- if (plot_type == "step") "#e31a1c" else "#e74c3c"

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = !!ggplot2::sym(x_var),
      y = !!ggplot2::sym(y_var)
    )
  )

  if (plot_type %in% c("line", "step")) {
    geom_fun <- if (plot_type == "line") ggplot2::geom_line else ggplot2::geom_step

    p <- p + geom_fun(color = color, linewidth = line_size)

    if (isTRUE(show_points)) {
      p <- p + ggplot2::geom_point(
        color = point_color,
        size = point_size
      )
    }
  }

  if (plot_type == "bar") {
    p <- p + ggplot2::geom_col(fill = color)
  }

  if (plot_type == "point") {
    p <- p + ggplot2::geom_point(
      color = color,
      size = point_size
    )
  }

  p <- p +
    ggplot2::theme_minimal(base_size = theme_base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(
        angle = rotate_x_angle,
        hjust = 1,
        vjust = if (rotate_x_angle == 0) 0.5 else 1
      )
    )

  if (inherits(data[[x_var]], c("Date", "POSIXct", "POSIXt"))) {
    p <- p + ggplot2::scale_x_date(
      date_breaks = date_breaks,
      date_labels = date_labels
    )
  }

  if (!is.null(y_suffix)) {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::label_number(suffix = y_suffix)
    )
  }

  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = x_label,
    y = y_label,
    caption = caption
  )

  return(p)
}
