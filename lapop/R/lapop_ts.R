#######################################

# LAPOP Time-Series Graphs #

#######################################


#' @include lapop_fonts.R
NULL

#'
#' LAPOP Time-Series Graphs
#'
#' This function creates time series graphs using LAPOP formatting.  If there are waves missing at the
#' beginning or end of the series, the function will omit those waves from the graph (i.e.,
#' the x-axis will range from the earliest wave for which data is supplied to the latest).  If there are
#' waves missing in the middle of the series, those waves will be displayed on the x-axis, but no data will be
#' shown.
#'
#' The input data must have a specific format to produce a graph.  It must include columns for
#' the survey wave (wave), the outcome variable (prop), the lower bound of the estimate (lb),
#' the upper bound of the estimate (ub), and a string for the outcome variable label (proplabel).
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled wave (survey wave/year; character vector), prop (outcome variable; numeric),
#' proplabel (text of outcome variable; character); lb (lower bound of estimate; numeric),
#' and ub (upper bound of estimate; numeric). Default: None (must be supplied).
#' @param wave_var,outcome_var,label_var,lower_bound,upper_bound,point_var Character, numeric, character,
#' numeric, numeric, character. Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: 0, 100.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the bottom-left corner of the graph. Default: LAPOP ("Source: LAPOP Lab" will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Percent of Mexicans who agree...".
#' Default: None.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  #' Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of lines and dots.  Takes hex number, beginning with "#".
#' Default: "#A43D6A" (red).
#' @param percentages Logical.  Is the outcome variable a percentage?  Set to FALSE if you are using
#' means of the raw values, so that the y-axis adjusts accordingly. Default: TRUE.
#' @param label_vjust Numeric. Customize vertical space between points and their labels.
#' Default: -2.1.
#' @param max_years Numeric. Threshold for automatic x-axis label rotation. When the number of unique
#' country labels exceeds this value, labels will be smaller and if necessary rotated for better readability.
#' Default: 15 years.
#' @param label_angle Numeric. Angle (in degrees) to rotate x-axis labels when max_years is exceeded. Default: 0.
#' @param ci_type Character. Controls how confidence intervals are displayed in the
#' time-series plot. This parameter only affects how the confidence interval is visualized;
#' the point estimate and line plot remain unchanged. Options:
#' \itemize{
#'   \item \code{"linerange"} (default): Draws upper and lower bounds as dashed lines.
#'   \item \code{"errorbar"}: Displays confidence intervals using vertical error bars
#'         centered on the point estimate.
#'   \item \code{"ribbon"}: Shows a shaded confidence band between the lower and
#'         upper bounds.
#'   \item \code{"none"}: Suppresses confidence interval display.
#' }
#'
#' @return Returns an object of class \code{ggplot}, a ggplot line graph showing
#' values of a variable over time.
#'
#' @examples
#' \donttest{
#' require(lapop); lapop_fonts()
#'
#' df <- data.frame(wave = c("2008", "2010", "2016/17", "2018/19", "2021"),
#' prop = c(23.2, 14.4, 35.8, 36.6, 40),
#' proplabel = c("23.2%", "14.4%", "35.8%", "36.6%", "40.0%"),
#' lb = c(20.2, 11.9, 33.3, 33.1, 38),
#' ub = c(26.2, 16.9, 38.3, 40.1, 42)
#' )
#'
#' lapop_ts(df,
#' main_title = "Ecuadorians are becoming more interested in politics",
#' subtitle = "% politically interested",
#' source_info = "Source: LAPOP Lab, AmericasBarometer Ecuador 2006-2021",
#' ymin = 0,
#' ymax = 55)
#'}
#'@export
#'@import ggplot2
#'@importFrom ggtext element_markdown
#'@importFrom zoo na.approx
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lapop_ts <- function(data, outcome_var = data$prop, lower_bound = data$lb,
                     upper_bound = data$ub, wave_var = as.character(data$wave),
                     label_var = data$proplabel, point_var = data$prop,
                     ymin = 0, ymax = 100, main_title = "",
                     source_info = "LAPOP", subtitle = "",
                     lang = "en", color_scheme = "#A43D6A",
                     percentages = TRUE, label_vjust = -2.1,
                     max_years = 15, label_angle = 0,
                     ci_type = "linerange") {   # <<< NEW PARAM

  rotate_labels <- length(unique(data$wave)) > max_years
  label_size = 5
  current_label_size <- ifelse(rotate_labels, (label_size * 0.5), label_size)

  if(sum(is.na(outcome_var)) > 0) {
    outcome_var = zoo::na.approx(outcome_var)
    lower_bound = zoo::na.approx(lower_bound)
    upper_bound = zoo::na.approx(upper_bound)
  }

  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme,
                          "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   ifelse(lang == "fr",
                          paste0(" <span style='color:", color_scheme,
                                 "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                                 "<span style='color:#585860; font-size:13pt'>Intervalle de confiance de 95% </span>"),
                          paste0(" <span style='color:", color_scheme,
                                 "; font-size:18pt'> \u2013 \u2013 \u2013</span> ",
                                 "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                                 "<span style='color:#585860'>interval</span>")))

  update_geom_defaults("text", list(family = "inter")) # roboto

  ts <- ggplot(data=data, aes(x=wave_var, y=outcome_var, group = 1)) +

    geom_line(color=color_scheme, linewidth = 1, alpha=0.48) +

  # CI TYPES
  # --------------------------------------------------------------
  {
    if (ci_type == "linerange") {
      list(
        geom_line(aes(y = lower_bound, group = 1),
                  color=color_scheme, linewidth = 1, alpha=0.48, linetype="dashed"),
        geom_line(aes(y = upper_bound, group = 1),
                  color=color_scheme, linewidth = 1, alpha=0.48, linetype="dashed")
      )
    } else if (ci_type == "errorbar") {
      geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                    width = 0.1, color = color_scheme, alpha = 0.75, linewidth = 1)
    } else if (ci_type == "ribbon") {
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                  fill = color_scheme, alpha = 0.48, color = NA)
    } else if (ci_type == "none") {
      NULL
    }
  } +

    geom_point(aes(y = point_var, color = " "), size = 3.5,
               alpha=0.48, key_glyph = "point") +

    scale_color_manual(values = color_scheme,
                       labels = paste0("<span style='color:#585860; font-size:13pt'> ",
                                       subtitle,
                                       "<span style='color:#FFFFFF00'>-----------</span>",
                                       ci_text)) +

    geom_text(aes(label = label_var), family = "inter", # roboto
              color=color_scheme, fontface="bold",
              size=5, vjust=label_vjust) +

    scale_x_discrete(limits = wave_var) +

    {
      if (percentages) {
        scale_y_continuous(limits=c(ymin, ymax),
                           breaks = seq(ymin, ymax,
                                        ifelse(ymax - ymin <= 50, 10, 20)),
                           labels = paste0(seq(ymin, ymax,
                                               ifelse(ymax - ymin <= 50, 10, 20)), "%"),
                           expand = c(0,0))
      } else {
        scale_y_continuous(limits=c(ymin, ymax), expand = c(0,0))
      }
    } +

    labs(title = main_title,
         caption = paste0(ifelse(lang == "es" & source_info == "LAPOP", "Fuente: LAPOP Lab",
                                 ifelse(lang == "en" & source_info == "LAPOP", "Source: LAPOP Lab",
                                        source_info))),
         x = " ", y = " ") +

    theme_minimal() +
    theme(text = element_text(size=14, family="inter"), # roboto
          plot.title = element_text(size=18, family="inter", face="bold"), # nunito
          plot.caption = element_text(size=10.5, vjust=2, hjust=0,
                                      family="inter", color="#585860"), # nunito
          axis.text = element_text(size = ifelse(rotate_labels, 10, 14),
                                   color="#585860"),
          panel.grid.major = element_line(color="#dddddf", linewidth=0.5),
          panel.grid.minor = element_line(color="#dddddf", linewidth=0.5),
          panel.border = element_rect(color="#dddddf", fill=NA, linewidth=1.0),
          legend.position="top",
          legend.title=element_blank(),
          legend.justification='left',
          legend.margin = margin(t=0, b=0, l=-40, r=0),
          legend.text = ggtext::element_markdown(family="inter-light")) # nunito-light

  if(rotate_labels) {
    ts <- ts +
      theme(axis.text.x = element_text(angle = label_angle,
                                       hjust = 0.5, vjust = 1)) +
      theme(plot.margin = margin(t=10, r=10,
                                 b=max(10, current_label_size*5), l=10))
  }

  return(ts)
}
