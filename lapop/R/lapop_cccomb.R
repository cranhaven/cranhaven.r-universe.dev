#######################################

# LAPOP Bar Graphs #

#######################################

#' @include lapop_fonts.R
NULL

#' LAPOP Bar Graphs
#'
#' This function shows a bar graph for categorical variables using LAPOP formatting.
#' @param cc1,cc2 lapop_cc (ggplot) object.  Graphic for left and right panes, respectively.
#' @param subtitle1,subtitle2 Character.  Describes the values/data shown in the graph, e.g., "Percent who agree that...".
#' Default: None.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: LAPOP Lab" in the bottom-left corner of the graph.
#' Default: None (only "Source: LAPOP Lab" will be printed).
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.
#' Takes hex numbers, beginning with "#". Default: "#008381".
#' @param file_name Character.  If desired, supply file path + name to save graph.
#' @param width_px,height_px Numeric.  Width and height of saved graph in pixels. Default: 895, 600.
#' @return Returns an object of class \code{ggplot}, a ggplot bar graph.
#'
#' @examples
#'\donttest{
#' require(lapop); lapop_fonts()
#' df1 <- data.frame(vallabel = c("Crime victim", "Non-victim"),
#' prop = c(36.1, 19.3),
#' proplabel = c("36%" ,"19%"),
#' lb = c(34.9, 18.1),
#' ub = c(37.3, 20.5))
#'
#' df2 <- data.frame(vallabel = c("Crime victim", "Non-victim"),
#' prop = c(45, 15),
#' proplabel = c("45%" ,"15%"),
#' lb = c(43, 13),
#' ub = c(47, 16))
#'
#' ccx <- lapop_cc(df1)
#' ccy <- lapop_cc(df2)
#'
#' lapop_cccomb(ccx, ccy,
#' subtitle1 = "% who support democracy",
#' subtitle2 = "% who are satisfied with democracy",
#' main_title = "Crime victims are more supportive of and satisfied with democracy",
#' source_info = ", AmericasBarometer 2023")
#'}
#'
#'@export
#'@import ggplot2
#'@import grid
#'@import gridtext
#'@importFrom gridExtra grid.arrange
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}


lapop_cccomb <- function(cc1, cc2,
                         subtitle1 = "",
                         subtitle2 = "",
                         main_title = "",
                         source_info = "",
                         lang = "en",
                         color_scheme = "#784885",
                         file_name = "",
                         width_px = 895,
                         height_px = 600
){
  cc1f = cc1 +
    theme(axis.title.x.bottom=element_text(size=13, family = "nunito"),
          plot.subtitle=element_text(size=14, family = "nunito", color = "#585860"),
          legend.position = "none") +
    labs(subtitle = subtitle1, caption = "")

  cc2f = cc2 +
    theme(axis.title.x.bottom=element_text(size=13, family = "nunito"),
          plot.subtitle=element_text(size=14, family = "nunito", color = "#585860"),
          legend.position = "none") +
    labs(subtitle = subtitle2, caption = "")


  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   ifelse(lang == "fr",
                          paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                                 "<span style='color:#585860; font-size:13pt'>Intervalle de confiance de 95% </span>"),
                          paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                                 "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                                 "<span style='color:#585860'>interval</span>")))
  # update_geom_defaults("text", list(family = "roboto"))

  top=textGrob(main_title,
               gp = gpar(fontfamily = "nunito", fontsize = 18, fontface = 'bold'),
                just = "left", x = 0)
  sub = gridtext::richtext_grob(ci_text, x = 0, hjust = -.05)
  bottom = textGrob(paste0("Source: LAPOP Lab", source_info),
                    gp = gpar(fontsize = 10.5, vjust = 2, hjust = 0.02, fontfamily = "nunito", color="#585860"),
                    just = "left", x = 0)
  g = c(list(top), list(sub), list(cc1f), list(cc2f), list(bottom))
  matlayout = rbind((1:1), (2:2), (3:4), (5:5))
  tab = gridExtra::grid.arrange(grobs=g,layout_matrix=matlayout,heights=c(1.4, 0.5, 10, 1.15))
  if(file_name != "") {
    ggsave(file_name, plot = tab, width = width_px/96, height = height_px/96)
  }
  return(invisible(tab))
}


