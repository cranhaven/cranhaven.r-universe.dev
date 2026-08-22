#######################################

# LAPOP Dumbbell Graphs #

#######################################


#' @rdname lapop-deprecated
#' @section \code{lapop_db}:
#' For \code{lapop_db}, use \code{\link{lapop_dumb}}.
#'
#' @export

lapop_db <- function(data,
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     main_title = "",
                     source_info = "",
                     subtitle = "",
                     sort = "wave2",
                     order = "hi-lo",
                     color_scheme = c("#482677", "#3CBC70"),
                     subtitle_h_just = 40,
                     subtitle_v_just = -18){
  .Deprecated("lapop_dumb")
  lapop_dumb(data = data,
             ymin = ymin,
             ymax = ymax,
             lang = lang,
             main_title = main_title,
             source_info = source_info,
             subtitle = subtitle,
             sort = sort,
             order = order,
             color_scheme = color_scheme,
             subtitle_h_just = subtitle_h_just,
             subtitle_v_just = subtitle_v_just)
}

#' @include lapop_fonts.R
NULL

#'
#' LAPOP Dummbell Graphs
#'
#' This function creates "dumbbell" graphs, which show averages for a variable across countries over two time periods, using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled pais (country name; character), wave1 (name of first wave/year (all rows are the same); character),
#' prop1 (outcome variable values for the first wave; numeric), proplabel1 (text of outcome variable for first wave; character),
#' wave2 (name of second wave/year (all rows are the same); character),
#' prop2 (outcome variable values for the second wave; numeric), proplabel2 (text of outcome variable for second wave; character).
#'  Default: None (must be supplied).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Defaults: 0 and 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: LAPOP (only "Source: "LAPOP Lab will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent who agree that...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of data points.  Must supply two values.  Takes hex numbers, beginning with "#".
#' Default: "#482677", "#3CBC70".
#' @param subtitle_h_just,subtitle_v_just Numeric.  Move the subtitle/legend text left/down (negative numbers) or right/up (positive numbers).
#' Ranges from -100 to 100.  Defaults: 40, -18.
#' @param sort Character.  The metric by which the data are sorted.  Options: "wave1" (outcome variable in first wave), "wave2" (outcome
#' variable in wave 2), "diff" (difference between the two waves), "alpha" (alphabetical by country name).
#' Default: "wave2".
#' @param order Whether data should be sorted from low to high or high to low on the sort metric.  Options: "hi-lo" and "lo-hi".
#' Default: "hi-lo".
#' @param text_nudge Numeric.  Move text of data further or closer to data point.  Default: 6.
#' @param drop_singles Logical.  Should rows with only one dot be removed?  Default: FALSE.
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' average values of some variable in two time periods across multiple countries
#'  (a dumbbell plot).
#'
#' @examples
#'\donttest{
#' require(lapop); lapop_fonts()
#' df <- data.frame(pais = c("Haiti", "Peru", "Honduras", "Colombia", "Ecuador",
#'                           "Panama", "Bolivia", "Argentina", "Paraguay",
#'                           "Dom. Rep.", "Brazil", "Jamaica", "Nicaragua",
#'                           "Guyana", "Costa Rica", "Mexico", "Guatemala",
#'                           "Chile", "Uruguay", "El Salvador"),
#'                  wave1 = rep("2018/19", 20),
#'                  prop1 = c(NA, 30, 58, 40, 49, 57, 33, 68, 38, 46, 30,
#'                            31, 70, NA, 43, 25, 38, 31, 34, 41),
#'                  proplabel1 = c(NA, "30%", "58%", "40%", "49%", "57%", "33%",
#'                                 "68%", "38%", "46%", "30%", "31%", "70%", NA,
#'                                 "43%", "25%", "38%", "31%", "34%", "41%"),
#'                  wave2 = rep("2021", 20),
#'                  prop2 = c(86, 73, 69, 67, 67, 65, 65, 65, 63, 62, 62,
#'                            57, 56, 56, 55, 55, 54, 51, 46, 42),
#'                  proplabel2 = c("86%", "73%", "69%", "67%", "67%", "65%", "65%",
#'                                 "65%", "63%", "62%", "62%", "57%", "56%", "56%",
#'                                 "55%", "55%", "54%", "51%", "46%", "42%"))
#'
#' lapop_dumb(df,
#' main_title = paste0("Personal economic conditions worsened across LAC"),
#'          subtitle = "% personal economic situation worsened",
#'          source_info = "Source: LAPOP Lab, AmericasBarometer 2018/19-2021")
#'}
#'
#'@export
#'@import ggplot2
#'@import ggtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_dumb <- function(data,
                      ymin = 0,
                      ymax = 100,
                      lang = "en",
                      main_title = "",
                      source_info = "",
                      subtitle = "",
                      sort = "wave2",
                      order = "hi-lo",
                      color_scheme = c("#008381", "#A43D6A"),
                      subtitle_h_just = 40,
                      subtitle_v_just = -18,
                      text_nudge = 6,
                      drop_singles = FALSE){

  # ---- drop_singles fix ----
  if (drop_singles) {
    data$usable1 <- !is.na(data$prop1) & data$prop1 != 0
    data$usable2 <- !is.na(data$prop2) & data$prop2 != 0
    data$usable_both <- data$usable1 & data$usable2
    data <- data[data$usable_both, , drop = FALSE]
  }

  if(sort == "diff"){
    data$diff = data$prop2-data$prop1
    data = data[order(-data$diff),]
  } else if(sort == "wave2"){
    data = data[order(-data$prop2),]
  } else if(sort == "wave1"){
    data = data[order(-data$prop1),]
  } else if(sort == "alpha"){
    data = data[order(data$pais),]
  }

  if(order == "hi-lo"){
    data$pais = factor(data$pais, levels = rev(unique(data$pais)))
  } else if(order == "lo-hi") {
    data$pais = factor(data$pais, levels = unique(data$pais))
  }

  data$max1 = data$prop2 < data$prop1

  data$max1[is.na(data$max1)] <- FALSE

  names(color_scheme) = c(unique(data$wave1), unique(data$wave2))

  update_geom_defaults("text", list(family = "inter")) # roboto
    ggplot(data, aes(y=pais)) +
      # connecting segment ONLY when both values exist
      ggplot2::geom_segment(
        ggplot2::aes(x = prop1, xend = prop2, y = pais, yend = pais),
        color = "#bdbdc3",
        linewidth = 1.5,
        lineend = "round"
      ) +
      geom_point(aes(x = prop1, color = names(color_scheme)[1]),
                 size=4, na.rm=T) +
      geom_point(aes(x = prop2, color = names(color_scheme)[2]),
                 size=4, na.rm=T) +
      geom_text(data = data[data$max1 == FALSE, ],
                aes(x = prop1, label = proplabel1),
                nudge_x = -text_nudge, color = color_scheme[1],
                size = 5, fontface = "bold", na.rm=T) +
      geom_text(data = data[data$max1 == TRUE, ],
                aes(x = prop1, label = proplabel1),
                nudge_x = text_nudge, color = color_scheme[1],
                size = 5, fontface = "bold", na.rm=T) +
      geom_text(data = data[data$max1 == TRUE, ],
                aes(x = prop2, label = proplabel2),
                nudge_x = -text_nudge, color = color_scheme[2],
                size = 5, fontface = "bold", na.rm=T) +
      geom_text(data = data[data$max1 == FALSE, ],
                aes(x = prop2, label = proplabel2),
                nudge_x = text_nudge, color = color_scheme[2],
                size = 5, fontface = "bold", na.rm=T) +
      geom_vline(xintercept = 0, color = "#dddddf") +
      scale_x_continuous(limits=c(ymin, ymax), breaks=seq(ymin, ymax, 20),
                         labels = paste(seq(ymin,ymax, 20), "%", sep=""),
                         expand = c(0,0)) +
      scale_color_manual(values = color_scheme) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = paste0(ifelse(lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
                                   ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
                            source_info))),
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "inter"), # roboto
            plot.title = element_text(size = 17, family = "inter", face = "bold"), # nunito
            plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2,
                                        family = "inter", color="#585860"), # nunito
            plot.subtitle = element_text(size = 14,
                                         family = "inter-light", color="#585860"), # nunito-light
            plot.margin = margin(5.5, 20, 5.5, 5.5, "points"),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            axis.text = element_text(size = 12,
                                     family = "inter", # roboto
                                     color = "#585860",
                                     margin=margin(r=5)),
            panel.grid.major.y = element_line(color = "#dddddf"),
            panel.background = element_rect(fill = "white"),
            legend.position="top",
            legend.text = element_text(family = "inter", color = "#585860"), # roboto
            legend.title = element_blank(),
            legend.justification='right',
            legend.key.size = unit(1, "line"),
            legend.key = element_blank(),
            legend.margin = margin(t=subtitle_v_just,b=0, subtitle_h_just, 0))
}





