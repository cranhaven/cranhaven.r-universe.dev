#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################

#' @include lapop_fonts.R
NULL

#' LAPOP Cross-Country Bar Graphs
#'
#' This function creates bar graphs for comparing values across countries using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled vallabel (values of x-axis variable (e.g. pais); character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character), lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#' @param vallabel,outcome_var,label_var,lower_bound,upper_bound Character, numeric, character,
#' numeric, numeric. Each component of the plot data can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column). x
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: 0 to 100.
#' @param highlight Character.  Country of interest.  Will highlight (make darker) that country's bar.
#' Input must match entry in "vallabel" exactly. Default: None.
#' @param sort Character. Method of sorting bars.  Options: "hi-lo" (highest to lowest y value), "lo-hi" (lowest to highest),
#' "alpha" (alphabetical by vallabel/x-axis label). Default: Order of data frame.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the bottom-left corner of the graph. Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "percentage of Mexicans who say...)".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.  Takes hex number, beginning with "#".
#' Default: #784885.
#' @param decimals Numeric. Number of decimals to display in default value labels.
#' Must be an integer from 0 to 3. Default: 0.
#' @param display_perc Logical. If TRUE, use `proplabel`-style labels (for example, with `%`).
#' If FALSE, use numeric `prop` values without the percent symbol. Default: TRUE.
#' @param set_x Numeric. Baseline from which bars should start. Must be a single
#' value between -100 and 100. Default: 0. When different from 0, bars are
#' plotted relative to this baseline and numeric axis labels are displayed
#' automatically.
#' @param text_nudge Numeric. Move text of data further from or closer to the
#' confidence interval. Default: 0.
#' @param label_size Numeric.  Size of text for data labels (percentages above bars).  Default: 5.
#' @param max_countries Numeric. Threshold for automatic x-axis label rotation. When the number of unique
#' country labels exceeds this value, labels will be rotated for better readability. Default: 20.
#' @param label_angle Numeric. Angle (in degrees) to rotate x-axis labels when max_countries is exceeded. Default: 0.
#' @param horizontal Logical. If TRUE, display bars horizontally. Default: FALSE.
#' @param display_y Logical. If TRUE, display numeric axis values. Default: FALSE.
#'
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' average values of some variables across multiple countries.
#'
#' @examples
#'\donttest{
#' require(lapop); lapop_fonts()
#'
#' df <- data.frame(
#' vallabel = c("PE", "CO", "BR", "PN", "GT", "DO", "MX", "BO", "EC"),
#' prop      = c(36.1, 19.3, 16.6, 13.3, 13.0, 11.1,  9.5,  9.0,  8.1),
#' proplabel = c("36%" ,"19%" ,"17%" ,"13%" ,"13%" ,"11%" ,"10%", "9%", "8%"),
#' lb        = c(34.9, 18.1, 15.4, 12.1, 11.8,  9.9,  8.3,  7.8,  6.9),
#' ub        = c(37.3, 20.5, 17.8, 14.5, 14.2, 12.3, 10.7, 10.2,  9.3)
#' )
#' lapop_cc(df,
#'          main_title = "Normalization of Intimate Partner Violence in LAC Countries",
#'          subtitle = "% who say domestic violence is private matter",
#'          source_info = "LAPOP Lab, AmericasBarometer 2021",
#'          highlight = "PE",
#'          decimals = 1,
#'          ymax = 50)
#' lapop_cc(df,
#'          main_title = "Normalization of Intimate Partner Violence in LAC Countries",
#'          subtitle = "% who say domestic violence is private matter",
#'          source_info = "LAPOP Lab, AmericasBarometer 2021",
#'          highlight = "PE",
#'          decimals = 1,
#'          ymax = 50,
#'          horizontal = TRUE)
#'}
#'@export
#'@import ggplot2
#'@import ggtext
#'@import sysfonts
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_cc <- function(data, outcome_var = data$prop, lower_bound = data$lb, vallabel = data$vallabel,
                     upper_bound = data$ub, label_var = data$proplabel,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     highlight = "",
                     main_title = "",
                     source_info = "LAPOP",
                     subtitle = "",
                     sort = "",
                     color_scheme = "#784885",
                     decimals = 0,
                     display_perc = TRUE,
                     set_x = 0,
                     text_nudge = 0,
                     label_size = 5,  # Default size
                     max_countries = 30,
                     label_angle = 0,
                     horizontal = FALSE,
                     display_y = FALSE) {  # Removed label_adjust

  if (!is.numeric(decimals) || length(decimals) != 1 || is.na(decimals) ||
      decimals %% 1 != 0 || decimals < 0 || decimals > 3) {
    stop("`decimals` must be a single integer between 0 and 3.")
  }
  if (!is.numeric(set_x) || length(set_x) != 1 || is.na(set_x) || set_x < -100 || set_x > 100) {
    stop("`set_x` must be a single numeric value between -100 and 100.")
  }

  if (missing(label_var)) {
    if (display_perc) {
      label_var <- data$proplabel
    } else {
    label_var <- sprintf(paste0("%.", decimals, "f"), outcome_var)
    }
  }

  # Check if we need to rotate labels based on number of unique countries
  rotate_labels <- length(unique(data$vallabel)) > max_countries
  # Adjust label size if rotation is needed
  current_label_size <- ifelse(rotate_labels, (label_size * 0.5), label_size)


  if(all(highlight != "")){
    data$hl_var = ifelse(vallabel %in% highlight, "hl", "other")
  }
  else{
    data$hl_var = "other"
  }
  data$bar_fill <- ifelse(data$hl_var == "hl", paste0(color_scheme, "47"), paste0(color_scheme, "20"))

  if(sort == "hi-lo"){
    data = data[order(-data$prop),]
  } else if(sort == "lo-hi"){
    data = data[order(data$prop),]
  } else if(sort == "alpha"){
    data = data[order(data$vallabel),]
  }

  data$x_index <- seq_len(nrow(data))
  data$prop_shifted <- data$prop - set_x
  data$lb_shifted <- lower_bound - set_x
  data$ub_shifted <- upper_bound - set_x
  data$bar_ymin <- pmin(0, data$prop_shifted)
  data$bar_ymax <- pmax(0, data$prop_shifted)
  data$label_y <- ifelse(data$prop_shifted < 0,
                         data$lb_shifted - text_nudge,
                         data$ub_shifted + text_nudge)
  data$label_vjust <- ifelse(data$prop_shifted < 0, 1.4, -0.5)
  data$label_hjust <- ifelse(data$prop_shifted < 0, 1, 0)
  show_value_axis <- isTRUE(display_y) || set_x != 0

  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   ifelse(lang == "fr",
                          paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                                 "<span style='color:#585860; font-size:13pt'>Intervalle de confiance de 95% </span>"),
                          paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                                 "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                                 "<span style='color:#585860'>interval</span>")))

  update_geom_defaults("text", list(family = "inter")) # roboto

  # Create base plot
   cc <- ggplot(data=data, aes(x = x_index, y = prop_shifted)) +
    geom_rect(aes(xmin = x_index - 0.3, xmax = x_index + 0.3, ymin = bar_ymin, ymax = bar_ymax),
              fill = data$bar_fill,
              color = color_scheme,
              inherit.aes = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.6, linetype = "solid", colour = "#dddddf") +
    geom_text(aes(label=label_var, y = label_y),
              vjust = if (horizontal) 0.5 else data$label_vjust,
              hjust = if (horizontal) data$label_hjust else 0.5,
              size=current_label_size, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes(ymin=lb_shifted, ymax=ub_shifted), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_x_continuous(
      breaks = data$x_index,
      labels = data$vallabel,
      expand = expansion(add = c(0.5, 0.5))
    ) +
    scale_y_continuous(
      limits = c(ymin - set_x, ymax - set_x),
      labels = function(x) x + set_x,
      expand = if (horizontal) expansion(mult = c(0.002, 0.08)) else expansion(mult = 0.002)
    ) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
                                 ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
                                        source_info)))) +
    theme(text = element_text(size = 14, family = "inter"), # roboto
          plot.title = element_text(size = 18, family = "inter", face = "bold"), # nunito
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0, family = "inter", color="#585860"), # nunito
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
          axis.text = element_text(size = ifelse(rotate_labels, 10, 14), color = "#585860", face = "bold"),
          axis.text.y = if (show_value_axis) {
            element_text(size = ifelse(rotate_labels, 10, 14), color = "#585860", face = "bold")
          } else {
            element_blank()
          },
          axis.ticks = element_blank(),
          plot.margin = if (horizontal) margin(t = 10, r = 40, b = 10, l = 10) else margin(t = 10, r = 10, b = 5.5, l = 5.5)) # nunito-light

  if (subtitle != "") {
    cc <- cc +
      annotate("text",
               x = -Inf, y = Inf,
               label = subtitle,
               hjust = -0.02, vjust = 1.8,
               family = "inter-light",
               size = 4.5,
               color = "#585860") +
      ggtext::geom_richtext(
        data = data.frame(x = Inf, y = Inf, label = ci_text),
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 1.02, vjust = 1.6,
        fill = NA, label.color = NA,
        family = "inter-light",
        size = 4.5
      )
  } else {
    cc <- cc +
      ggtext::geom_richtext(
        data = data.frame(x = Inf, y = Inf, label = ci_text),
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = 1.02, vjust = 1.6,
        fill = NA, label.color = NA,
        family = "inter-light",
        size = 4.5
      )
  }

  # Apply label rotation if needed
  if(rotate_labels && !horizontal) {
   cc <- cc + theme(axis.text.x = element_text(angle = label_angle,
                                              hjust = 0.5, vjust = 1))

    # Adjust plot margins to accommodate rotated labels
     cc <-  cc + theme(plot.margin = margin(t = 10, r = 10, b = max(10, current_label_size*5), l = 10))
  }

  if (horizontal) {
    cc <- cc + coord_flip(clip = "off")
  }

  return(cc)
}
