#######################################

# LAPOP Stacked Bar Graph #

#######################################

#' @rdname lapop-deprecated
#' @section \code{lapop_sb}:
#' For \code{lapop_sb}, use \code{\link{lapop_stack}}.
#'
#' @export

lapop_sb <- function(data, outcome_var = data$prop, prop_labels = data$proplabel,
                        var_labels = data$varlabel, value_labels = data$vallabel,
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        source_info = "",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        hide_small_values = TRUE,
                        display_perc = TRUE,
                        order_bars = FALSE,
                        subtitle_h_just = 0,
                        color_scheme = c("#2D708E", "#1F9689", "#00ADA9", "#21A356", "#568424", "#ACB014")){
  .Deprecated("lapop_stack")
  lapop_stack(data = data, outcome_var = outcome_var, prop_labels = prop_labels,
              var_labels = var_labels, value_labels = value_labels,
              lang = lang,
              main_title = main_title,
              subtitle = subtitle,
              source_info = source_info,
              rev_values = rev_values,
              rev_variables = rev_variables,
              hide_small_values = hide_small_values,
              display_perc = display_perc,
              order_bars = order_bars,
              subtitle_h_just = subtitle_h_just,
              color_scheme = color_scheme)
}


#' @include lapop_fonts.R
NULL

#' LAPOP Stacked Bar Graphs
#'
#' This function shows a stacked bar graph using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled varlabel (name(s)/label(s) of variable(s) of interest; character), vallabel (names/labels of values for each variable; character),
#' prop (outcome variable value; numeric), and proplabel (text of outcome variable value; character).
#' Default: None (must be provided).
#' @param outcome_var,prop_labels,var_labels,value_labels Numeric, character, character, character.
#' Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param xvar Logical. If `TRUE`, group the plots using the `xvar_label` column in the dataset.
#' If `FALSE`, do not group. Default: `FALSE`.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who support...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of data bars for each value.  Allows up to 6 values.
#' Takes hex numbers, beginning with "#".
#' Default: c("#2D708E", "#008381", "#C74E49", "#784885", "#a43d6a","#202020")
#' (navy blue, turquoise, teal, green, sap green, pea soup).
#' @param subtitle_h_just Numeric.  Move the subtitle/legend text left (negative numbers) or right (positive numbers).
#' Ranges from -100 to 100.  Default: 0.
#' @param fixed_aspect_ratio Logical.  Should the aspect ratio be set to a specific value (0.35)?
#' This prevents bars from stretching vertically to fit the plot area.  Set to false when you have
#' a large number of bars (> 10).  Default: TRUE.
#' @param size_aspect_ratio Numeric. Optional custom bar thickness to use when
#' `fixed_aspect_ratio = FALSE`. Default: `NULL`.
#' @param rev_variables Logical.  Should the order of the variables be reversed?  Default: FALSE.
#' @param rev_values Logical.  Should the order of the values for each variable be reversed?  Default: FALSE.
#' @param hide_small_values Logical.  Should labels for categories with 3 percent or less be hidden?  Default: TRUE.
#' @param display_perc Logical. If `TRUE`, use `proplabel`-style labels (for example, with `%`).
#' If `FALSE`, use numeric `prop` values without the percent symbol. Default: TRUE.
#' @param vallab_size Numeric. Size of the percentage labels inside the bars.
#' Default: 5.
#' @param order_bars Logical.  Should categories be reordered automatically
#' based on their values?  Default: FALSE.
#' @param legendnrow Numeric.  How many rows for legend labels. Default: 1.
#' @return Returns an object of class \code{ggplot}, a ggplot stacked bar graph
#'
#' @examples
#' \donttest{
#'df <- data.frame(varlabel = c(rep("Politicians can\nidentify voters", 5),
#'                              rep("Wealthy can\nbuy results", 5),
#'                              rep("Votes are\ncounted correctly", 5)),
#'                 vallabel = rep(c("Always", "Often", "Sometimes",
#'                                  "Never", "Other"), 3),
#'                 prop = c(36, 10, 19, 25, 10, 46, 10, 23, 11, 10, 35,
#'                          10, 32, 13, 10),
#'                 proplabel = c("36%", "10%", "19%", "25%", "10%", "46%",
#'                               "10%", "23%", "11%", "10%", "35%", "10%",
#'                               "32%", "13%", "10%"))
#'require(lapop); lapop_fonts()
#'lapop_stack(df,
#'         main_title = "Trust in key features of the electoral process is low in Latin America",
#'         subtitle = "% believing it happens:",
#'         source_info = "Source: LAPOP Lab, AmericasBarometer 2019")
#'}
#'@export
#'@import ggplot2
#'@import ggtext
#'@import showtext
#'@importFrom stats ave
#'@importFrom stats reorder

#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_stack <- function(data,
                        outcome_var = data$prop,
                        prop_labels = data$proplabel,
                        var_labels = data$varlabel,
                        value_labels = data$vallabel,
                        xvar = FALSE,
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        source_info = "LAPOP",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        hide_small_values = TRUE,
                        display_perc = TRUE,
                        order_bars = FALSE,
                        subtitle_h_just = 0,
                        fixed_aspect_ratio = TRUE,
                        size_aspect_ratio = NULL,
                        vallab_size = 5,
                        legendnrow = 1,
                        color_scheme = c("#2D708E", "#008381", "#C74E49", "#784885", "#a43d6a", "#202020")) {

  if (!inherits(var_labels, "character") & !inherits(var_labels, "factor")) {
    var_labels = as.character(var_labels)
    data$varlabels = as.character(data$varlabel)
  }
  if (!inherits(value_labels, "character") & !inherits(value_labels, "factor")) {
    value_labels = as.character(value_labels)
    data$vallabel = as.character(data$vallabel)
  }

  plot_data <- data.frame(
    var_labels = var_labels,
    value_labels = value_labels,
    outcome_var = outcome_var,
    prop_labels = prop_labels
  )

  plot_data$label_text <- if (isTRUE(display_perc)) {
    as.character(plot_data$prop_labels)
  } else {
    as.character(plot_data$outcome_var)
  }

  if (isTRUE(xvar)) {
    if ("xvar_label" %in% colnames(data)) {
      plot_data$group_var <- data[["xvar_label"]]
    } else {
      warning("Column `xvar_label` not found in data. Define `xvar` in `lpr_stack()` to create `xvar_label`. Ignoring grouping.")
      xvar <- FALSE
    }
  } else {
    xvar <- FALSE
  }

  if (inherits(value_labels, "factor")) {
    value_levels <- levels(value_labels)
  } else {
    value_levels <- unique(value_labels)
  }

  if (rev_values == TRUE) {
    value_levels <- rev(value_levels)
  }

  plot_data$value_labels <- factor(plot_data$value_labels, levels = value_levels)

  mycolors <- color_scheme[seq_along(value_levels)]
  names(mycolors) <- value_levels
  bar_width <- if (isTRUE(fixed_aspect_ratio) || is.null(size_aspect_ratio)) 0.6 else size_aspect_ratio

  if (isTRUE(xvar)) {
    plot_data$combined_label <- plot_data$group_var

    if (rev_variables) {
      positions <- rev(unique(plot_data$combined_label))
    } else {
      positions <- unique(plot_data$combined_label)
    }

    plot_data$x_display <- plot_data$combined_label
  } else {
    if (rev_variables) {
      positions <- rev(unique(plot_data$var_labels))
    } else {
      positions <- unique(plot_data$var_labels)
    }

    plot_data$x_display <- plot_data$var_labels
  }

  group_totals <- ave(plot_data$outcome_var, plot_data$x_display, FUN = sum)
  plot_data$plot_value <- ifelse(
    group_totals > 0,
    plot_data$outcome_var / group_totals * 100,
    plot_data$outcome_var
  )

  update_geom_defaults("text", list(family = "inter"))

  if (order_bars == TRUE) {
    if (isTRUE(xvar)) {
      plot_data$x_display <- factor(plot_data$x_display)

      plot <- ggplot(
        plot_data,
        aes(
          y = plot_value,
          x = x_display,
          fill = reorder(value_labels, outcome_var),
          label = label_text
        )
      )
    } else {
      plot_data$var_labels <- factor(plot_data$var_labels, levels = unique(plot_data$var_labels))
      plot_data$x_display <- plot_data$var_labels

      plot <- ggplot(
        plot_data,
        aes(
          y = plot_value,
          x = x_display,
          fill = reorder(value_labels, outcome_var),
          label = label_text
        )
      )
    }

    plot +
      geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = bar_width) +
      geom_text(
        aes(label = ifelse(outcome_var > 3, label_text, NA)),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "#FFFFFF",
        fontface = "bold",
        size = vallab_size,
        na.rm = TRUE
      ) +
      ggrepel::geom_text_repel(
        aes(label = ifelse(outcome_var <= 3 & hide_small_values == FALSE, label_text, NA)),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "#FFFFFF",
        segment.color = "transparent",
        fontface = "bold",
        size = 4,
        family = "inter",
        direction = "y",
        force_pull = 0.2,
        force = 5,
        na.rm = TRUE
      ) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide = guide_legend(reverse = FALSE, nrow = legendnrow), na.translate = FALSE) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(
        title = main_title,
        y = "",
        x = " ",
        caption = paste0(
          ifelse(
            lang == "es" & source_info == "LAPOP",
            "Fuente: LAPOP Lab",
            ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab", source_info)
          )
        ),
        subtitle = subtitle
      ) +
      theme(
        text = element_text(size = 14, family = "inter"),
        plot.title = element_text(size = 17, family = "inter", face = "bold"),
        plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color = "#585860"),
        plot.subtitle = element_text(size = 14, family = "inter-light", color = "#585860"),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
          size = 14,
          family = "inter",
          color = "#585860",
          hjust = 1,
          vjust = 0.5,
          margin = margin(r = 5)
        ),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14, family = "inter", color = "#585860"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "top",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.text = element_text(family = "inter", color = "#585860"),
        legend.title = element_blank(),
        legend.justification = "left",
        legend.key.size = unit(1, "line"),
        legend.margin = margin(t = 5, b = 5, 0, subtitle_h_just)
      ) +
      {
        if (fixed_aspect_ratio) {
          theme(aspect.ratio = 0.35)
        }
      }
  } else {
    ggplot(plot_data, aes(fill = value_labels, y = plot_value, x = x_display, label = label_text)) +
      geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = bar_width) +
      geom_text(
        aes(label = ifelse(outcome_var > 3, label_text, NA)),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "#FFFFFF",
        fontface = "bold",
        size = vallab_size,
        na.rm = TRUE
      ) +
      ggrepel::geom_text_repel(
        aes(label = ifelse(outcome_var <= 3 & hide_small_values == FALSE, label_text, NA)),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "#FFFFFF",
        segment.color = "transparent",
        fontface = "bold",
        size = 4,
        family = "inter",
        direction = "y",
        force_pull = 0.2,
        force = 5,
        na.rm = TRUE
      ) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide = guide_legend(reverse = FALSE, nrow = legendnrow)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(
        title = main_title,
        y = "",
        x = " ",
        caption = paste0(
          ifelse(
            lang == "es" & source_info == "LAPOP",
            "Fuente: LAPOP Lab",
            ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab", source_info)
          )
        ),
        subtitle = subtitle
      ) +
      theme(
        text = element_text(size = 14, family = "inter"),
        plot.title = element_text(size = 17, family = "inter", face = "bold"),
        plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, family = "inter-light", color = "#585860"),
        plot.subtitle = element_text(size = 14, family = "inter-light", color = "#585860"),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
          size = 14,
          family = "inter",
          color = "#585860",
          hjust = 1,
          vjust = 0.5,
          margin = margin(r = 5)
        ),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14, family = "inter", color = "#585860"),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.position = "top",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.text = element_text(family = "inter", color = "#585860"),
        legend.title = element_blank(),
        legend.justification = "left",
        legend.key.size = unit(1, "line"),
        legend.margin = margin(t = 5, b = 5, 0, subtitle_h_just)
      ) +
      {
        if (fixed_aspect_ratio) {
          theme(aspect.ratio = 0.35)
        }
        }
  }
}
