#######################################

# LAPOP Bar Graphs #

#######################################

#' @include lapop_fonts.R
NULL

#' LAPOP Bar Graphs
#'
#' This function shows a bar graph for categorical variables using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled cat (labels of each category in variable; character),
#' prop (outcome variable value; numeric), and proplabel (text of outcome variable value; character).
#' Default: None (must be provided).
#' @param cat_var,outcome_var,label_var Character, numeric, character.
#' Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Defaults: 0, 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the bottom-left corner of the graph. Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who agree that...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.
#' Takes hex numbers, beginning with "#". Default: "#008381".
#' @param order Logical.  Should bars be ordered from most frequent response to least?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot bar graph.
#'
#' @examples
#' \donttest{
#' require(lapop); lapop_fonts()
#'
#' df <- data.frame(
#' cat = c("Far Left", 1, 2, 3, 4, "Center", 6, 7, 8, 9, "Far Right"),
#' prop = c(4, 3, 5, 12, 17, 23, 15, 11, 5, 4, 1),
#' proplabel = c("4%", "3%", "5%", "12%", "17%", "23%", "15%", "11%", "5%", "4%", "1%")
#' )
#' lapop_hist(df,
#'            main_title = "Centrists are a plurality among Peruvians",
#'            subtitle = "Distribution of ideological preferences",
#'            source_info = "Source: LAPOP Lab, AmericasBarometer Peru 2019",
#'            ymax = 27)
#'}
#'@export
#'@import ggplot2
#'@import ggtext
#'@importFrom stringr str_wrap
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_hist <- function(data, outcome_var = data$prop, label_var = data$proplabel,
                       cat_var = data$cat,
                       ymin = 0,
                       ymax = 100,
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "LAPOP",
                       order = FALSE,
                       color_scheme = "#008381"){
  if(order == TRUE){
    data = data[order(-data$prop), ]
    cat_var = cat_var[order(-outcome_var)]
    label_var = label_var[order(-outcome_var)]
    outcome_var = outcome_var[order(-outcome_var)]
  }
  update_geom_defaults("text", list(family = "inter")) # roboto
  ggplot(data, aes(x=factor(cat_var, levels = cat_var), y = outcome_var)) +
    geom_bar(stat = "identity", color = color_scheme, fill = paste0(color_scheme, "28"), width = 0.75) +
    geom_text(aes(label=label_var), vjust=-0.5, size = 5, fontface = "bold", color = color_scheme) +
    scale_x_discrete(labels = function(x) {
      has_manual_breaks <- grepl("<br>|\n", x)
      normalized_labels <- gsub("<br>", "\n", x)

      ifelse(
        has_manual_breaks,
        normalized_labels,
        stringr::str_wrap(x, width = 24)
      )
    }) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    labs(title=main_title,
         y = "",
         x = "",
         caption = paste0(ifelse(lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
                                 ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
                                        source_info))),
         subtitle = subtitle) +
    theme(text = element_text(size = 14, family = "inter"), # roboto
          plot.title = element_text(size = 18, family = "inter", face = "bold"), # nunito
          plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, family = "inter", color="#585860"), # nunito
          plot.subtitle = element_text(size = 13, family = "inter-light", color="#585860", margin=margin(b = 10)), # nunito
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          axis.text = element_text(size = 14, family = "inter-light", color = "#585860"), # roboto
          panel.grid = element_line(color = "#dddddf"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank())
}
