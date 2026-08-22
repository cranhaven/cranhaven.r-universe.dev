#######################################

# LAPOP Multiple-Over/Breakdown Graph #

#######################################

#' @rdname lapop-deprecated
#' @section \code{lapop_demog}:
#' For \code{lapop_demog}, use \code{\link{lapop_mover}}.
#'
#' @export

lapop_demog <- function(data,
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        source_info = "",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        subtitle_h_just = 0,
                        ymin = 0,
                        ymax = 100,
                        x_lab_angle = 90,
                        color_scheme = c("#7030A0", "#00ADA9", "#3CBC70", "#7EA03E", "#568424", "#ACB014")){
  .Deprecated("lapop_mover")
  lapop_mover(data = data,
  lang = lang,
  main_title = main_title,
  subtitle = subtitle,
  qword = NULL,
  source_info = source_info,
  rev_values = rev_values,
  rev_variables = rev_variables,
  subtitle_h_just = subtitle_h_just,
  ymin = ymin,
  ymax = ymax,
  x_lab_angle = x_lab_angle,
  color_scheme = color_scheme)
}

#' @include lapop_fonts.R
NULL

#' LAPOP Multiple-Over/Breakdown Graphs
#'
#' This function shows the values of an outcome variable for subgroups of a secondary variable, using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled varlabel (name(s)/label(s) of secondary variable(s); character), vallabel (names/labels of values for secondary variable; character),
#' prop (outcome variable value; numeric), proplabel (text of outcome variable value; character),
#' lb (lower bound of estimate; numeric), and ub (upper bound of estimate; numeric).
#' Default: None (must be provided).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Defaults: 0 and 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the bottom-left corner of the graph. Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who agree that...".
#' Default: None.
#' @param qword Character.  Describes the question wording shown in the graph, e.g., "Do you agree that...".
#' Default: NULL.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of data points and text for each secondary variable.  Allows up to 6 values.
#' Takes hex numbers, beginning with "#".
#' Default: c("#784885", "#008381", "#c74e49", "#2d708e", "#a43d6a")
#' (purple, teal, green, olive, sap green, pea soup).
#' @param subtitle_h_just Numeric.  Move the subtitle/legend text left (negative numbers) or right (positive numbers).
#' Ranges from -100 to 100.  Default: 0.
#' @param x_lab_angle Numeric.  Angle/orientation of the value labels.  Default: 90.
#' @param rev_variables Logical.  Should the order of the variables be reversed?  Default: FALSE.
#' @param rev_values Logical.  Should the order of the values for each variable be reversed?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' average values of some variable broken down by one or more secondary variables
#' (commonly, demographic variables).
#'
#' @examples
#'\donttest{
#' df <- data.frame(varlabel = c(rep("Gender", 2), rep("Age", 6),
#'                               rep("Education", 4), rep("Wealth", 5)),
#'                  vallabel = c("Women", "Men", "18-25", "26-35", "36-45",
#'                               "46-55", "56-65", "66+", "  None", "Primary",
#'                               "Secondary", "Post-Sec.", "Low", "2",
#'                               "3", "4", "High"),
#'                  prop = c(20, 22, 21, 24, 22, 21, 17, 15, 20, 18, 21, 25, 21,
#'                           21, 21, 21, 22),
#'                  proplabel = c("20%", "22%", "21%", "24%", "22%", "21%",
#'                                "17%", "15%", "20%", "18%", "21%", "25%",
#'                                "21%", "21%", "21%", "21%", "22%"),
#'                  lb = c(19, 21, 20, 23, 21, 20, 15, 13, 16, 17, 20, 24, 20,
#'                         20, 20, 20, 21),
#'                  ub = c(21, 23, 22, 25, 23, 22, 19, 17, 24, 19, 22, 26, 22,
#'                         22, 22, 22, 23))
#'require(lapop); lapop_fonts
#' lapop_mover(df,
#'             main_title = paste0("More educated, men, and younger individuals",
#'                                 " in the LAC region are the\nmost likely",
#'                                   " to be crime victims"),
#'             subtitle = "% victim of a crime", qword = "",
#'             source_info = "Source: LAPOP Lab, AmericasBarometer",
#'             ymin = 0,
#'             ymax = 40)
#'}
#'@export
#'@import ggplot2
#'@import stringr
#'@importFrom ggtext element_markdown
#'@importFrom stats setNames
#'@import showtext
#'@importFrom stringr str_wrap
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_mover <- function(data,
                        lang = "en",
                        main_title = "",
                        subtitle = "",
                        qword = NULL,
                        source_info = "LAPOP",
                        rev_values = FALSE,
                        rev_variables = FALSE,
                        subtitle_h_just = 0,
                        ymin = 0,
                        ymax = 100,
                        x_lab_angle = 90,
                        color_scheme = c("#784885", "#008381", "#c74e49", "#2d708e", "#a43d6a")) {

  data$varlabel = factor(data$varlabel, levels = unique(data$varlabel))
  data$order = 1:nrow(data)
  data$order = factor(data$order, levels = unique(data$order))
  mycolors = color_scheme[seq_along(unique(data$varlabel))]

  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:#585860; font-size:18pt'>\u0131\u2014\u0131 </span>",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   ifelse(lang == "fr",
                          paste0(" <span style='color:#585860; font-size:18pt'>\u0131\u2014\u0131 </span>",
                                 "<span style='color:#585860; font-size:13pt'>Intervalle de confiance de 95% </span>"),
                          paste0(" <span style='color:#585860; font-size:18pt'> \u0131\u2014\u0131 </span> ",
                                 "<span style='color:#585860; font-size:13pt'>95% confidence interval</span>")))

  update_geom_defaults("text", list(family = "inter")) # roboto

  # Build per-facet vertical line positions based on the number of values in each varlabel
  vline_df <- do.call(rbind, lapply(split(data, data$varlabel), function(df) {
    n <- nrow(df)
    data.frame(
      varlabel = unique(df$varlabel)[1],
      xint = if (n > 1) seq(0.5, n - 0.5, by = 1) else numeric(0)
    )
  }))

  # Create a color map based on unique levels in varlabel
  color_map <- setNames(mycolors[1:length(unique(data$varlabel))], unique(data$varlabel))

  # Modify the facet labeller function to use colors
  grid_labeller <- function(x) {
    # Respect user-supplied line breaks before applying automatic wrapping
    wrapped_labels <- ifelse(
      grepl("<br>|\\n", x),
      gsub("\\n", "<br>", x),
      str_wrap(x, width = 12)
    )

    # Apply color from the color_map based on the variable level
    color <- color_map[x]

    # Apply color line-by-line so wrapped facet labels keep the same color
    wrapped_lines <- strsplit(wrapped_labels, "<br>|\\n")
    colored_labels <- vapply(seq_along(wrapped_lines), function(i) {
      paste0(
        "<span style='color:", color[i], "'>",
        wrapped_lines[[i]],
        "</span>",
        collapse = "<br>"
      )
    }, character(1))

    return(colored_labels)
  }

  ggplot(data, aes(x = order, y = prop, color = factor(varlabel), label = proplabel)) +
    geom_point(alpha = 0.47, key_glyph = "point") +
    facet_grid(. ~ varlabel,
               scales = "free_x",
               space = "free_x",
               labeller = labeller(varlabel = grid_labeller)) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2, show.legend = FALSE) +
    geom_text(aes(y = ub), fontface = "bold", size = 5, vjust = -0.8, show.legend = FALSE) +
    scale_color_manual(values = mycolors,
                       labels = paste0("<span style='color:#585860; font-size:13pt'> ",
                                       subtitle,
                                       "<span style='color:#FFFFFF00'>-----------</span>",
                                       ci_text),
                       guide = guide_legend(override.aes = list(shape = 16,
                                                                color = c("black", rep("white", length(unique(data$varlabel)) - 1)),
                                                                fill = c("black", rep("white", length(unique(data$varlabel)) - 1))))) +
    scale_y_continuous(limits = c(ymin, ymax),
                       breaks = seq(ymin, ymax, ifelse(ymax - ymin <= 50, 5, 10)),
                       expand = c(0, 0)) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(data$vallabel[match(x, data$order)], width = 12),
      expand = expansion(add = 0.5)
    ) +
    geom_vline(
      data = vline_df,
      aes(xintercept = xint),
      color = "#dddddf",
      linewidth = 0.5,
      show.legend = FALSE) +
    labs(title = main_title,
         y = "",
         x = " ",
         caption = paste0(ifelse(lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
                                 ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
                                        source_info)))) +
    theme(text = element_text(size = 14, family = "inter"), # roboto
          plot.title = element_text(size = 17, family = "inter", face = "bold"), # nunito
          plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, family = "inter", color = "#585860"), # nunito
          plot.subtitle = element_text(size = 14, family = "inter", color = "#585860"), # nunito
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.5, color = "#dddddf"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(linetype = "solid", color = "#dddddf", fill = NA, linewidth = 1),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = x_lab_angle, vjust = 0.5),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "inter", color = "#585860"), # roboto
          legend.position = "top",
          legend.title = element_blank(),
          legend.justification = 'left',
          legend.margin = margin(0, 0, -5, 0 - subtitle_h_just),
          legend.text = element_markdown(family = "inter-light"), # nunito
          legend.key = element_blank(),
          strip.text = element_markdown(size = 14),
          strip.background = element_blank()
    )
}

