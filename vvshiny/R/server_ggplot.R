#' Wrapped chart
#'
#' Wrapped chart function for creating a plot based on the provided dataframe and variables.
#'
#' @param df The data frame used to create the plot.
#' @param x The variable used on the x-axis of the plot.
#' @param y The variable used on the y-axis of the plot.
#' @param color The variable used to color the points or bars in the plot.
#' @param id The identifier for selecting the data frame source.
#' @param df_original The original dataframe before summarization
#' @param y_left The variable used on the left y-axis when creating a comparative plot.
#' @param y_right The variable used on the right y-axis when creating a comparative plot.
#' @param facet_var The variable used for facet wrapping.
#' @param facet_name_var The name of the variable used for facet wrapping.
#'
#' @return A ggplot object.
wrapped_chart <- function(df, x, y, color, id = "bench", df_original, y_left = NULL, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {
  ## Depending on the type of plot, set facet wrap and labels settings
  if (stringr::str_detect(id, "bench")) {
    facet_wrap_setting <- ggplot2::facet_wrap(ggplot2::vars(!!facet_name_var))
    xlab_setting <- ggplot2::xlab(display_name(x, id))
    ylab_setting <- ggplot2::ylab(display_name(y, id))

    ## Get boolean vars in order to add formatting %
    if (is.logical(df_original[[y]])) {
      scale_y <- ggplot2::scale_y_continuous(labels = scales::percent)
    } else {
      scale_y <- ggplot2::scale_y_continuous()
    }
  } else if (stringr::str_detect(id, "comp")) {
    if (is.logical(df_original[[y_left]]) && is.logical(df_original[[y_right]])) {
      scale_y <- ggplot2::scale_y_continuous(labels = scales::percent)
    } else {
      scale_y <- ggplot2::scale_y_continuous()
    }


    facet_wrap_setting <- ggplot2::facet_wrap(ggplot2::vars(!!facet_var),
      scales = "free_y",
      labeller = ggplot2::as_labeller(c(
        "left" = display_name(y_left, id),
        "right" = display_name(y_right, id)
      ))
    )
    xlab_setting <- ggplot2::xlab(NULL)
    ylab_setting <- ggplot2::ylab(NULL)
  }

  ## Create the base plot
  plot <- basic_plot(df, x, y, color, xlab_setting, ylab_setting, ggplot_basic_settings(), "top", scale_y) +
    facet_wrap_setting

  ## Set plot type based on the nature of x
  if (is.numeric(df[[x]]) && length(unique(df[[x]])) > 1) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(color = !!rlang::sym(color))) +
      ggplot2::geom_point(ggplot2::aes(color = !!rlang::sym(color))) +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(accuracy = 1),
        n.breaks = length(unique(df[[x]]))
      )
    ## Set breaks instead of n_breaks when there is only value in numeric variable
  } else if (is.numeric(df[[x]])) {
    plot <- plot + ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(accuracy = 1),
        breaks = unique(df[[x]])
      )
  } else {
    plot <- plot + ggplot2::geom_bar(position = "dodge", stat = "identity")
  }

  ## Add legend using ggplotly
  plot <- ggplotly_with_legend(plot, color, id)

  return(plot)
}

#' Stacked bar chart
#'
#' Create a stacked bar chart, with optional settings for percentage (or not) and wrap (or not) modes.
#'
#' @param df The data frame used to create the plot.
#' @param x The variable used on the x-axis of the plot.
#' @param color The variable used to color the points or bars in the plot.
#' @param id The identifier for selecting the data frame source.
#' @param facet_name_var The name of the variable used for facet wrapping.
#' @param percentage Logical indicating whether to create a plot in percentage mode.
#' @param wrap Logical indicating whether to use facet wrapping.
#'
#' @return A ggplot object.
stacked_composition_bar_chart <- function(df, x, color, id, facet_name_var = rlang::sym("VIS_Groep_naam"), percentage = FALSE, wrap = FALSE) {
  Aantal <- NULL

  ## Set plot type and y-axis label depending on whether plot is in percentage mode
  if (percentage == TRUE) {
    position <- "fill"
    y <- "Percentage"
    scale_y <- ggplot2::scale_y_continuous(labels = scales::percent)
    df <- df %>%
      dplyr::rename(Percentage = Aantal)
  } else {
    position <- "stack"
    y <- "Aantal"
    scale_y <- ggplot2::scale_y_continuous()
  }

  xlab_setting <- ggplot2::xlab(display_name(x, id))
  ylab_setting <- ggplot2::ylab(y)

  ## Create the base plot
  plot <- basic_plot(df, x, y, color, xlab_setting, ylab_setting, ggplot_basic_settings()) +
    ggplot2::geom_bar(position = position, stat = "identity") +
    scale_y

  if (is.numeric(df[[x]]) && length(unique(df[[x]])) > 1) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(accuracy = 1),
        n.breaks = length(unique(df[[x]]))
      )
  } else if (is.numeric(df[[x]])) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        labels = scales::label_comma(accuracy = 1),
        breaks = unique(df[[x]])
      )
  }

  if (wrap == TRUE) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!facet_name_var))
  }

  plot <- plotly::ggplotly(plot)

  return(plot)
}


#' Grid boxplot
#'
#' Function for creating grid boxplots for either benchmark or comparison across variables.
#'
#' @param df The data frame used to create the plot.
#' @param x The variable used on the x-axis of the plot.
#' @param color The variable used to color the points or bars in the plot.
#' @param y The variable used on the y-axis of the plot.
#' @param id The identifier for selecting the data frame source.
#' @param y_left The variable used on the left y-axis when creating a comparative plot.
#' @param y_right The variable used on the right y-axis when creating a comparative plot.
#' @param facet_var The variable used for facet wrapping.
#' @param facet_name_var The name of the variable used for facet wrapping.
#'
#' @return A ggplot object.
grid_boxplots <- function(df, x, color, y, id, y_left = NULL, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {
  df <- df %>%
    dplyr::arrange(!!rlang::sym(color), !!rlang::sym(x)) %>%
    dplyr::mutate(!!rlang::sym(y) := as.numeric(!!rlang::sym(y)))

  unique_values_x <- unique(df[[x]])
  n_rows_grid <- length(unique_values_x)

  ## Set the facet grid and labels settings depending on the type of plot
  if (stringr::str_detect(id, "bench")) {
    facet_grid_setting <- ggplot2::facet_grid(
      rows = ggplot2::vars(!!rlang::sym(x)),
      cols = ggplot2::vars(!!facet_name_var)
    )
    xlab_setting <- ggplot2::xlab(display_name(x, id))
    ylab_setting <- ggplot2::ylab(display_name(y, id))


    ## Create base plot
    plot <- basic_plot(df, color, y, color, xlab_setting, ylab_setting, ggplot_basic_settings()) +
      ggplot2::geom_boxplot() +
      facet_grid_setting

    plot <- plotly::ggplotly(plot, height = 250 * n_rows_grid)
  } else if (stringr::str_detect(id, "comp")) {
    xlab_setting <- ggplot2::xlab(NULL)
    ylab_setting <- ggplot2::ylab(NULL)

    plot_list <- list()
    ## Create a plot for each unique value of x
    for (value_x in (unique_values_x)) {

      ## Filter only for this value of x
      df_part <- df %>% dplyr::filter(!!rlang::sym(x) == value_x)


      ## Set labels dynamic as names for facet_var since labeller() function doesn't accept
      ## Non Standard Evaluation (i.e. use of !!)
      labels_facet_var <- stats::setNames(
        list(c(
          "left" = paste(display_name(y_left, id), value_x, sep = " - "),
          "right" = paste(display_name(y_right, id), value_x, sep = " - ")
        )),
        as.character(facet_var)
      )

      facet_grid_setting <- ggplot2::facet_wrap(
        ggplot2::vars(!!facet_var),
        scales = "free_y",
        labeller = ggplot2::labeller(!!!labels_facet_var)
      )
      scale_x <- ggplot2::scale_x_discrete()

      ## Create base plot
      subplot <- basic_plot(
        df_part,
        color,
        y,
        color,
        xlab_setting,
        ylab_setting,
        ggplot_basic_settings()
      ) +
        ggplot2::geom_boxplot() +
        facet_grid_setting +
        scale_x +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(accuracy = 0.1)
        )

      ## Ggplot object is itself a list. Wrap this in a list to get a list with ggplot list objects
      plot_list <- append(plot_list, list(subplot))
    }

    plot_list <- purrr::map(plot_list, ~ plotly::ggplotly(.x, height = 250 * n_rows_grid))

    plot <- plotly::subplot(plot_list, nrows = n_rows_grid)
  }



  return(plot)
}

#' Grid histogram
#'
#' Function for creating grid histograms.
#'
#' @param df The data frame used to create the plot.
#' @param x The variable used on the x-axis of the plot.
#' @param color The variable used to color the points or bars in the plot.
#' @param y The variable used on the y-axis of the plot.
#' @param id The identifier for selecting the data frame source.
#' @param y_left The variable used on the left y-axis when creating a comparative plot.
#' @param y_right The variable used on the right y-axis when creating a comparative plot.
#' @param facet_var The variable used for facet wrapping.
#' @param facet_name_var The name of the variable used for facet wrapping.
#'
#' @return A list of ggplot objects.
grid_histograms <- function(df, x, color, y, id, y_left = NULL, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {
  width <- density <- NULL

  df <- df %>%
    dplyr::mutate(!!rlang::sym(y) := as.numeric(!!rlang::sym(y)))

  unique_values_x <- unique(df[[x]])
  n_rows_grid <- length(unique_values_x)
  ylab_setting <- ggplot2::ylab(NULL)
  scale_y <- ggplot2::scale_y_continuous(labels = scales::percent)

  scale_x <- ggplot2::scale_x_continuous() # limits = c(min(df[[y]], na.rm = TRUE), max(df[[y]], na.rm = TRUE)))

  ## Set the facet grid and labels settings depending on the type of plot
  if (stringr::str_detect(id, "bench")) {
    xlab_setting <- ggplot2::xlab(display_name(y, id))

    facet_grid_setting <- ggplot2::facet_grid(
      rows = ggplot2::vars(!!rlang::sym(x)),
      cols = ggplot2::vars(!!facet_name_var)
    )


    ## Create base plot
    plot <- basic_plot(df, y, y, color, xlab_setting, ylab_setting, ggplot_basic_settings()) +
      ggplot2::geom_histogram(
        position = "identity",
        ## The bar overlap so alpha is needed to see through
        alpha = 0.5,
        ggplot2::aes(color = !!rlang::sym(color), y = ggplot2::stat(width * density))
      ) +
      facet_grid_setting +
      scale_y +
      scale_x

    ## Make plotly with appropriate height and put it in list for further usage
    plot <- plotly::ggplotly(plot, height = 250 * n_rows_grid)
    plot_list <- list(plot)
  } else if (stringr::str_detect(id, "comp")) {
    xlab_setting <- ggplot2::xlab(NULL)

    plot_list <- list()
    ## Create a plot for each unique value of x
    for (value_x in (unique_values_x)) {
      ## Filter only for this value of x
      df_part <- df %>% dplyr::filter(!!rlang::sym(x) == value_x)


      ## Set labels dynamic as names for facet_var since labeller() function doesn't accept
      ## Non Standard Evaluation (i.e. use of !!)
      labels_facet_var <- stats::setNames(
        list(c(
          "left" = paste(display_name(y_left, id), value_x, sep = " - "),
          "right" = paste(display_name(y_right, id), value_x, sep = " - ")
        )),
        as.character(facet_var)
      )

      facet_grid_setting <- ggplot2::facet_wrap(
        ggplot2::vars(!!facet_var),
        scales = "free_x",
        labeller = ggplot2::labeller(!!!labels_facet_var)
      )

      ## Create base plot
      subplot <- basic_plot(df_part, y, y, color, xlab_setting, ylab_setting, ggplot_basic_settings()) +
        ggplot2::geom_histogram(
          position = "identity",
          ## The bar overlap so alpha is needed to see through
          alpha = 0.5,
          ggplot2::aes(color = !!rlang::sym(color), y = ggplot2::stat(width * density))
        ) +
        facet_grid_setting +
        scale_y +
        scale_x

      ## Ggplot object is itself a list. Wrap this in a list to get a list with ggplot list objects
      plot_list <- append(plot_list, list(subplot))
    }

    ## Make plotlys of all the ggplots and add total height
    plot_list <- purrr::map(plot_list, ~ plotly::ggplotly(.x, height = 250))
  }

  return(plot_list)
}


#' Create a Gantt plot using ggplot and plotly
#'
#' This function creates a Gantt plot with the help of ggplot and plotly.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x A string specifying the column name to be used as the x-axis variable.
#' @param xend A string specifying the column name to be used as the end of the x-axis variable.
#' @param split_var A string specifying the column name to be used as the splitting variable.
#' @param title A string specifying the title of the plot.
#' @param position_label_y A string specifying the position of y-axis labels.
#' @return A Gantt plot.
gantt_plot <- function(df, x, xend, split_var, title, position_label_y) {

  plot <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = !!rlang::sym(x),
        xend = !!rlang::sym(xend),
        # y = !!rlang::sym(split_var),
        # yend = !!rlang::sym(split_var),
        y = stats::reorder(!!rlang::sym(split_var), !!rlang::sym(x), decreasing = TRUE),
        yend = stats::reorder(!!rlang::sym(split_var), !!rlang::sym(x), decreasing = TRUE),
        color = !!rlang::sym(split_var)
      )
    ) +
      ## worden. Dit kan worden getest door de 'plots' pane groter / kleine te maken
      ggplot2::geom_segment(size = 6) +
      ggpubr::theme_pubr() +
      # ggplot_basic_settings() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::scale_x_continuous(labels = scales::label_percent()) +
      ggplot2::ggtitle(title) +
      ## Daarom extra plotly layout code toegevoegd
      ggplot2::scale_y_discrete(position = position_label_y)

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    rlang::inform("When the package RColorBrewer is installed, brewer palette Set3 is used")
  }

  plot <- plot +
    ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(name = "Set3", n = nrow(df)) %>% purrr::set_names(df[[split_var]]))

    plotly::ggplotly(plot) %>%
    plotly::layout(
      yaxis = list(side = position_label_y),
      legend =
        list(
          # orientation = "h",
          # xanchor = "center",
          # x = 0.5,
          # y = 1.20,
          title = title
        )
    )
}


#' Create a Sankey plot using ggplot and ggalluvial
#'
#' This function creates a Sankey plot with the help of ggplot and ggalluvial.
#'
#' @param df A data frame containing the data to be plotted.
#' @param left_var A string specifying the column name to be used as the left variable.
#' @param right_var A string specifying the column name to be used as the right variable.
#' @param xlab_setting ggplot labels settings for x axes.
#' @param ylab_setting ggplot labels settings for y axes.
#' @param name_left A string specifying the name for the left side of the plot.
#' @param name_right A string specifying the name for the right side of the plot.
#' @param title A string specifying the title of the plot.
#' @param title_size Numeric value specifying the size of the title.
#' @param title_font A string specifying the font of the title.
#' @return A Sankey plot.
sankey_plot <- function(df, left_var, right_var, xlab_setting, ylab_setting, name_left, name_right, title, title_size = 20, title_font = "verdana") {
  n <- stratum <- NULL

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      axis1 = !!rlang::sym(left_var),
      axis2 = !!rlang::sym(right_var),
      y = n
    )
  ) +
    ggplot2::scale_x_discrete(limits = c(name_left, name_right), expand = c(.2, .05)) +
    xlab_setting +
    ylab_setting +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = c(!!rlang::sym(left_var))), alpha = 0.75, width = 1 / 8) +
    ggalluvial::geom_stratum(na.rm = FALSE, alpha = 1, width = 1 / 8) +
    ggplot2::geom_text(stat = "stratum", ggplot2::aes(label = ggplot2::after_stat(stratum))) +
    ggpubr::theme_pubr() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, family = "verdana")) +
    ggplot2::ggtitle(title) +
    ggplot2::guides(fill = "none")
}


#' Set ggplot basic settings
#'
#' Basic ggplot settings are put in a list and returned
#'
#' @return A list with ggplot settings
ggplot_basic_settings <- function() {
  settings <- list(ggplot2::scale_fill_brewer(palette = "Pastel1"), ggplot2::scale_colour_brewer(palette = "Pastel1"))
  return(settings)
}



#' Create a basic plot using ggplot
#'
#' This function creates a basic plot with the help of ggplot.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x A string specifying the column name to be used as the x-axis variable.
#' @param y A string specifying the column name to be used as the y-axis variable.
#' @param color A string specifying the column name to be used as the fill variable.
#' @param xlab_setting ggplot labels settings for x axes.
#' @param ylab_setting ggplot labels settings for y axes.
#' @param ggplot_settings Additional settings for the ggplot.
#' @param legend_position A string specifying the position of the legend.
#' @param scale_y Optional ggplot2 scale function to modify the y axis.
#' @return A ggplot plot.
#' @export
#' @examples
#' df <- data.frame(x_var = rnorm(100),
#'                  y_var = rnorm(100),
#'                  color_var = sample(c("Red", "Blue"),
#'                  100,
#'                  replace = TRUE))
#' xlab_setting <- ggplot2::xlab("x label")
#' ylab_setting <- ggplot2::ylab("y label")
#' ggplot_instellingen <- ggplot2::geom_point()
#' scale_y <- ggplot2::scale_y_continuous()
#' basic_plot(df, "x_var", "y_var", "color_var", xlab_setting,
#'            ylab_setting, ggplot_instellingen, "none", scale_y)
basic_plot <- function(df, x, y, color, xlab_setting, ylab_setting, ggplot_settings = ggplot_basic_settings(), legend_position = "none", scale_y = NULL) {
  plot <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = !!rlang::sym(x),
      y = !!rlang::sym(y),
      fill = !!rlang::sym(color)
    )
  ) +
    ggpubr::theme_pubr() +
    ggplot_settings +
    xlab_setting +
    ylab_setting +
    ggplot2::theme(legend.position = legend_position)

  if (!is.null(scale_y)) {
    plot <- plot + scale_y
  }

  return(plot)
}
