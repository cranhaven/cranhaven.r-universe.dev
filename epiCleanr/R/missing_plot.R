#' Plot Missing data over time
#'
#' This function visualizes the proportion of missing data or reporting rate for
#' specified variables in a dataset. It creates a tile plot using
#' \code{\link[=ggplot2]{ggplot2}}; where the x-axis can
#' represent any categorical time such as time (e.g., year, month), and the
#' y-axis can represents either variables or groupings (e.g., state). The
#' output can further be manipulated to one's needs.
#'
#' @param data A data frame containing the data to be visualized. Must include
#' columns specified in 'x_var', 'y_var', and 'vars'.
#' @param x_var A character string specifying the time variable in 'data'
#' (e.g., "year", "month"). Must be provided.
#' @param y_var An optional character string specifying the grouping
#' variable in 'data' (e.g., "state"). If provided, only one variable can be
#' specified in 'vars'.
#' @param  miss_vars An optional character vector specifying the variables
#' to be visualized in 'data'. If NULL, all variables except 'x_var' and
#' 'y_var' will be used.
#' @param use_rep_rate A logical value. If TRUE, the reporting rate is
#' visualized; otherwise, the proportion of missing data is visualized.
#' Defaults to FALSE
#' @return A ggplot2 object representing the tile plot.
#'
#' @export
#'
#' @examples
#'
#'
#' # get path
#' path <- system.file(
#'         "extdata",
#'         "fake_epi_df_togo.rds",
#'          package = "epiCleanr")
#'
# # get example data
#' fake_epi_df_togo <- import(path)
#'
#' # Check misisng data by year
#' result <- missing_plot(fake_epi_df_togo,
#'              x_var = "year", use_rep_rate = FALSE)
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_bw
#' theme scale_x_discrete scale_y_discrete guides guide_legend unit
#' @importFrom dplyr select mutate across group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with all_of
#' @importFrom stringr str_remove
#' @importFrom tools toTitleCase
#' @importFrom rlang sym .data

missing_plot <- function(data, x_var, y_var = NULL,
                         miss_vars = NULL, use_rep_rate = FALSE) {
  # Check if 'x_var' is provided and exists in the data
  if (is.null(x_var) || !x_var %in% names(data)) {
    stop("A valid 'x_var' must be provided and must exist in the data.")
  }

  # If 'y_var' is provided, ensure only one variable is specified in 'miss_vars'
  if (!is.null(y_var) && length(miss_vars) != 1) {
    stop(paste(
      "When 'y_var' is provided",
      "only one variable can be specified in 'miss_vars'."
    ))
  }

  # Determine the fill variable and label based on 'use_rep_rate'
  fill_var <- ifelse(use_rep_rate, "rep_rate", "propmiss")
  fill_label <- ifelse(use_rep_rate, "Reporting rate (%)", "Missing rate (%)")

  # Determine y-axis label based on 'y_var'
  y_axis_label <- if (!is.null(y_var)) {
    tools::toTitleCase(y_var)
  } else {
    "Variable"
  }

  # Construct the title based on the provided parameters
  title_prefix <- if (use_rep_rate) {
    "Reporting rate of"
  } else {
    "The proportion of missing data for"
  }

  max_vars_in_title <- 5 # Set a threshold

  title_vars <- if (!is.null(miss_vars)) {
    paste(paste(miss_vars, collapse = ", "), "by", x_var)
  } else {
    remaining_vars <- setdiff(names(data), c(x_var, y_var))
    if (length(remaining_vars) <= max_vars_in_title) {
      paste(
        paste(remaining_vars[-length(remaining_vars)], collapse = ", "),
        "and",
        remaining_vars[length(remaining_vars)],
        "by",
        x_var
      )
    } else {
      paste("various variables by", x_var)
    }
  }

  # If 'vars' is not provided, use all variables
  # except 'x_var' and 'y_var'
  if (is.null(miss_vars)) {
    miss_vars <- setdiff(names(data), c(x_var, y_var))
  }

  title_suffix <- if (!is.null(y_var)) paste("and", y_var) else ""

  # Select relevant columns and mutate missing values
  plot_data <- data |>
    dplyr::select(tidyselect::all_of(c(x_var, y_var, miss_vars))) |>
    dplyr::mutate(dplyr::across(
      .cols = -c(
        !!rlang::sym(x_var),
        if (!is.null(y_var)) tidyselect::all_of(y_var) else NULL
      ),
      .fns = ~ ifelse(is.na(.), 1, 0),
      .names = "miss_{.col}"
    ))

  # Pivot the missing value columns to long format
  plot_data <- plot_data |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("miss_"),
      names_to = "variable",
      values_to = "miss_value"
    ) |>
    dplyr::mutate(variable = stringr::str_remove(.data$variable, "miss_"))

  # If 'y_var' is not NULL, group by 'x_var', 'y_var', and 'variable'
  if (!is.null(y_var)) {
    plot_data <- plot_data |>
      dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(y_var), .data$variable)
  } else {
    plot_data <- plot_data |>
      dplyr::group_by(!!rlang::sym(x_var), .data$variable)
  }

  # Summarize missing data
  plot_data <- plot_data |>
    dplyr::summarise(
      miss = sum(.data$miss_value, na.rm = TRUE),
      tot = dplyr::n(), .groups = 'drop'
    ) |>
    dplyr::mutate(
      propmiss = .data$miss / .data$tot * 100,
      rep_rate = 100 - .data$propmiss
    )

  # Determine the y-axis variable based on the presence of 'y_var'
  if (!is.null(y_var)) {
    y_axis_var <- as.name(y_var)
  } else {
    y_axis_var <- "variable"
  }


  # reverse colours if reporting rate is used
  if (use_rep_rate) {
    color_pal <-  rev(wesanderson::wes_palette("Zissou1", 100,
                                               type = "continuous"))
  } else {
    color_pal <-
      wesanderson::wes_palette("Zissou1", 100, type = "continuous")
  }


  # Plot the data using ggplot2
  plot <- ggplot2::ggplot(
    plot_data,
    aes(
      x = as.factor(!!rlang::sym(x_var)),
      y = !!as.name(y_axis_var), fill = !!rlang::sym(fill_var)
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = .2) +
    ggplot2::scale_fill_gradientn(
      colours = color_pal
    ) +
    ggplot2::labs(
      title = trimws(paste(title_prefix, title_vars, title_suffix)),
      x = "", y = y_axis_label, fill = fill_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        size = 12, face = "bold",
        family = "Arial"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, unit = "pt")
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10, unit = "pt")
      ),
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      legend.margin = ggplot2::margin(t = 0, unit = "cm"),
      legend.text = ggplot2::element_text(size = 8, family = "Arial"),
      plot.title = ggplot2::element_text(
        size = 12,
        # face = "bold",
        family = "Arial",
        margin = ggplot2::margin(b = 10)
      ),
      axis.text = ggplot2::element_text(family = "Arial"),
      axis.title = ggplot2::element_text(family = "Arial"),
      strip.text = ggplot2::element_text(family = "Arial"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top", nrow = 1,
      label.position = "bottom", direction = "horizontal",
      key.height = ggplot2::unit(1, "lines"),
      key.width = ggplot2::unit(1, "lines")
    ))

  return(plot)
}

