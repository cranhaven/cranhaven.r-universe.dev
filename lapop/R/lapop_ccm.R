#######################################

# LAPOP Cross-Country Bar Graphs #

#######################################

#' @include lapop_fonts.R
NULL

#' LAPOP Cross-Country Bar Graphs
#'
#' This function creates bar graphs for comparing values across countries using LAPOP formatting.
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled pais (values of x-axis variable (usually pais); character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character), lb (lower bound of estimate; numeric),
#'  ub (upper bound of estimate; numeric), and var (labels of secondary variables; character).
#'  Default: None (must be supplied).
#' @param pais,outcome_var,label_var,lower_bound,upper_bound,var Character, numeric, character,
#' numeric, numeric, character. Each component of the plot data can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: 0 to 100.
#' @param sort Character. Method of sorting bars.  Options: "var1" (highest to lowest on variable 1),
#' "var2" (highest to lowest on variable 2), "var3" (highest to lowest on variable 3),
#' "var4" (highest to lowest on variable 4),
#' "alpha" (alphabetical along x-axis/pais). Default: Order of data frame.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: None (only "Source: " will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "percentage of Mexicans who say...)".
#' Default: None.
#' @param y_label Character.  Y-axis label.
#' @param x_label Character.  X-axis label.
#' @param highlight Character.  Country of interest.  Will highlight (make darker) that country's bar.
#' Input must match entry in "vallabel" exactly. Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.  Takes hex number, beginning with "#".
#' Default: "#784885", "#008381", "#C74E49", "#2D708E".
#' @param label_size Numeric.  Size of text for data labels (percentages above bars).  Default: 4.
#' @param text_position Numeric.  Amount that text above error bars should be offset (to avoid overlap).  Default: 0.7
#' @param horizontal Logical. If TRUE, display the grouped bars horizontally. Default: FALSE.
#' @param display_y Logical. If TRUE, display numeric axis values. Default: FALSE.
#'
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' average values of some variables across multiple countries.
#'
#' @examples
#' \donttest{
#' require(lapop); lapop_fonts()
#'
#' df <- data.frame(pais = c(rep("HT", 2), rep("PE", 2), rep("HN", 2), rep("CO", 2),
#'              rep("UY", 2), rep("CR", 2), rep("EC", 2), rep("CL", 2),
#'               rep("BR", 2), rep("BO", 2), rep("JA", 2), rep("PN", 2)),
#'               var = rep(c("countfair1", "countfair3"), 3),
#'               prop = c(30, 38, 40, 49, 57, 33, 80, 54, 30, 43, 61, 42,
#'                        38, 54, 74, 61, 50, 34, 48, 34, 72, 41, 58, 57),
#'               proplabel = c("30%", "38%", "40%", "49%", "57%", "33%",
#'                             "80%", "54%", "30%", "43%", "61%", "42%",
#'                             "38%", "54%", "74%", "61%", "50%", "34%",
#'                             "48%", "34%", "72%", "41%", "58%", "57%"),
#'               lb = c(27, 35, 37, 46, 54, 30, 77, 51, 27, 40, 58, 39,
#'                      35, 51, 71, 58, 47, 31, 45, 31, 69, 38, 55, 54),
#'               ub = c(33, 41, 43, 52, 60, 36, 83, 57, 33, 46, 64, 45,
#'                      41, 57, 77, 64, 53, 37, 51, 37, 75, 44, 61, 60))
#'
#' lapop_ccm(df, sort = "var", source_info = ", AmericasBarometer")
#' lapop_ccm(df, sort = "var", source_info = ", AmericasBarometer", horizontal = TRUE)
#'}
#'@export
#'@import ggplot2
#'@import dplyr
#'@import ggtext
#'@import sysfonts
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_ccm <- function(data,
                      pais = data$pais, outcome_var = data$prop,
                      lower_bound = data$lb, upper_bound = data$ub,
                      label_var = data$proplabel, var = data$var,
                      ymin = 0,
                      ymax = 100,
                      lang = "en",
                      main_title = "",
                      source_info = "",
                      subtitle = "",
                      sort = "",
                      y_label = "",
                      x_label = "",
                      highlight = "",
                      color_scheme = c("#784885", "#008381", "#C74E49", "#2D708E"),
                      label_size = 4,
                      text_position = 0.7,
                      horizontal = FALSE,
                      display_y = FALSE) {

  data$pais <- pais
  data$prop <- outcome_var
  data$lb <- lower_bound
  data$ub <- upper_bound
  data$proplabel <- label_var
  data$var_group <- as.character(var)
  data$var_label <- data$var_group

  if (length(unique(data$var_group)) > 4) {
    stop("`lapop_ccm()` supports a maximum of 4 variables.")
  }

  if (length(color_scheme) < length(unique(data$var_group))) {
    stop("`color_scheme` must have at least as many colors as the number of variables being plotted.")
  }

  var_levels <- unique(data$var_group)

  # Define highlight logic
  if (highlight != "") {
    data$hl_var <- ifelse(data$pais == highlight, "hl", "other")
  } else {
    data$hl_var <- "other"
  }

  # Compute numeric alpha values (no warning!)
  data$alpha_value <- ifelse(data$hl_var == "hl", 0.6, 0.32)

  # Add language-specific legend label customization
  if (lang == "es") {
    data$var_label <- ifelse(data$var_group == var_levels[length(var_levels)],
                             paste0(data$var_group,
                                    "<span style='color:#FFFFFF00'>-------</span>",
                                    "<span style='color:#585860; font-size:18pt'> \u0131\u2014\u0131</span>",
                                    "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                             data$var_group)
  } else if (lang == "fr") {
    data$var_label <- ifelse(data$var_group == var_levels[length(var_levels)],
                             paste0(data$var_group,
                                    "<span style='color:#FFFFFF00'>-------</span>",
                                    "<span style='color:#585860; font-size:18pt'> \u0131\u2014\u0131</span>",
                                    "<span style='color:#585860; font-size:13pt'>Intervalle de confiance de 95% </span>"),
                             data$var_group)
  } else {
    data$var_label <- ifelse(data$var_group == var_levels[length(var_levels)],
                             paste0(data$var_group,
                                    "<span style='color:#FFFFFF00'>-------</span>",
                                    "<span style='color:#585860; font-size:18pt'> \u0131\u2014\u0131</span>",
                                    "<span style='color:#585860; font-size:13pt'>95% confidence interval </span>"),
                             data$var_group)
  }

  var_label_levels <- data$var_label[match(var_levels, data$var_group)]
  data$var_label <- factor(data$var_label, levels = var_label_levels)
  fill_colors = setNames(paste0(color_scheme[seq_along(var_levels)], "52"), var_label_levels)
  line_colors = setNames(color_scheme[seq_along(var_levels)], var_label_levels)

  # Sorting logic
  if (sort == "var1") {
    data <- data %>%
      group_by(var_group) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(var_group, rank)
  } else if (sort == "var2") {
    data <- data %>%
      group_by(var_group) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(var_group, var_levels[2]), rank)
  } else if (sort == "var3") {
    data <- data %>%
      group_by(var_group) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(var_group, var_levels[3]), rank)
  } else if (sort == "var4") {
    data <- data %>%
      group_by(var_group) %>%
      mutate(rank = rank(-prop)) %>%
      arrange(match(var_group, var_levels[4]), rank)
  } else if (sort == "alpha") {
    data <- data[order(data$pais), ]
  }

  data$label_position <- ifelse(data$prop < 0, data$lb - text_position, data$ub + text_position)
  data$label_vjust <- ifelse(data$prop < 0, 1.4, -0.5)
  data$label_hjust <- ifelse(data$prop < 0, 1, 0)

  # Apply font
  update_geom_defaults("text", list(family = "inter")) # roboto

  axis_labels <- if (horizontal) {
    list(x = y_label, y = x_label)
  } else {
    list(x = x_label, y = y_label)
  }

  dodge_pos <- position_dodge(width = 0.7)

  p <- ggplot(data = data,
              aes(x = factor(pais, levels = unique(pais)),
                  y = prop,
                  fill = var_label,
                  color = var_label,
                  group = var_label)) +
    geom_bar(aes(alpha = alpha_value), position = dodge_pos, stat = "identity", width = 0.7) +
    geom_text(
      aes(label = proplabel, y = label_position),
      position = dodge_pos,
      vjust = if (horizontal) 0.5 else data$label_vjust,
      hjust = if (horizontal) data$label_hjust else 0.5,
      size = label_size,
      fontface = "bold",
      show.legend = FALSE
    ) +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                  width = 0.15,
                  position = dodge_pos, linetype = "solid", show.legend = FALSE) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = line_colors) +
    scale_y_continuous(
      limits = c(ymin, ymax),
      expand = if (horizontal) expansion(mult = c(0.002, 0.08)) else expansion(mult = c(0.002, 0.03))
    ) +
    labs(title = main_title,
         y = axis_labels$y,
         x = axis_labels$x,
         caption = paste0(ifelse(lang == "es", "Fuente: LAPOP Lab", "Source: LAPOP Lab"),
                          source_info)) +
    { if (subtitle != "") labs(subtitle = subtitle) } +
    { if (!horizontal && x_label != "") theme(axis.title.x = element_text(margin = margin(b = 10, t = 10))) } +
    { if (horizontal && y_label != "") theme(axis.title.x = element_text(margin = margin(b = 10, t = 10))) } +
    theme(text = element_text(size = 14, family = "inter"), # roboto
          plot.title = element_text(size = 18, family = "inter", face = "bold"), # nunito
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0, family = "inter", color = "#585860"), # nunito
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
          axis.text = element_text(size = 14, color = "#585860", face = "bold"),
          axis.text.y = if (display_y) {
            element_text(size = 14, color = "#585860", face = "bold")
          } else {
            element_blank()
          },
          axis.text.x = element_text(size = 14, color = "#585860", face = "bold"),
          axis.ticks = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification = 'left',
          legend.margin = margin(t = 0, b = 0, l = 0),
          plot.margin = if (horizontal) margin(t = 10, r = 50, b = 10, l = 10) else margin(t = 10, r = 10, b = 10, l = 10),
          legend.text = ggtext::element_markdown(family = "inter-light")) + guides(alpha = "none")

  if (horizontal) {
    p <- p + coord_flip(clip = "off")
  }

  p
}
