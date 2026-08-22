#######################################

# LAPOP Regression Coefficient Graphs #

#######################################

#' @include lapop_fonts.R
NULL
#'
#' LAPOP Regression Graphs
#'
#' This function creates plots of regression coefficients and predicted probabilities using LAPOP formatting.
#'
#' @param data Data Frame. Dataset to be used for analysis.  The data frame should have columns
#' titled coef (regression coefficients/predicted probabilities; numeric), proplabel (text of outcome variable; character),
#' varlabel (names of variables to be plotted; character), lb (lower bound of coefficient estimate; numeric),
#' ub (upper bound of estimate; numeric), and pvalue (p value of coefficient estimate; numeric).
#'  Default: None (must be supplied).
#' @param coef_var,label_var,varlabel_var,lb,ub,pval_var Numeric, character, character, numeric,
#' numeric, numeric. Each component of the data to be plotted can be manually specified in case
#' the default columns in the data frame should not be used (if, for example, the values for a given
#' variable were altered and stored in a new column).
#' @param ymin,ymax Numeric.  Minimum and maximum values for y-axis. Default: dynamic.
#' @param main_title Character.  Title of graph.  Default: None.
#' @param source_info Character.  Information on dataset used (country, years, version, etc.),
#' which is added to the end of "Source: " in the bottom-left corner of the graph.
#' Default: None (only "Source: " will be printed).
#' @param subtitle Character.  Describes the values/data shown in the graph, e.g., "Regression coefficients".
#' Default: automatic text based on `pred_prob`.
#' @param sort Character. The metric by which the coefficients are sorted. Options:
#' `"coef"` (coefficient values) or `"alpha"` (alphabetical by variable label).
#' Default: `"coef"`.
#' @param order Character. Whether data should be sorted from low to high or high to low
#' on the selected sort metric. Options: `"hi-lo"` and `"lo-hi"`. Default: `"hi-lo"`.
#' @param lang Character.  Changes default subtitle text and source info to either Spanish or English.
#' Will not translate input text, such as main title or variable labels.  Takes either "en" (English)
#' or "es" (Spanish).  Default: "en".
#' @param color_scheme Character.  Color of bars.  Takes hex number, beginning with "#".
#' Default: "#784885" (purple).
#' @param pred_prob Logical.  Is the graph showing predicted probabilities (instead of regression coefficients)?
#' Will only change text in the legend, not the data.  Default: FALSE.
#' @param dot_size Numeric. Size of the dots. Default: 5.5.
#' @param coef_label_size Numeric. Size of the coefficient labels. Default: 5.
#' @param subtitle_h_just Numeric.  Move the subtitle/legend text left (negative numbers) or right (positive numbers).
#' Ranges from -100 to 100.  Default: 0.
#' @return Returns an object of class \code{ggplot}, a ggplot figure showing
#' coefficients or predicted probabilities from a multivariate regression.
#'
#' @examples
#' \donttest{
#' require(lapop); lapop_fonts()
#' df <- data.frame(
#'   varlabel = c("Intimate\nPartner", "wealth", "Education", "Age", "Male"),
#'   coef = c(0.02, -0.07, -0.24, 0.01, 0.11),
#'   lb = c(-0.002, -0.110, -0.295, -0.060, 0.085),
#'   ub = c(0.049, -0.031, -0.187, 0.080, 0.135),
#'   pvalue = c(0.075, 0.000, 0.000, 0.784, 0.000),
#'   proplabel = c("0.02", "-0.07", "-0.24", "0.01", "0.11")
#' )
#'
#' lapop_coef(df,
#'            main_title = "Demographic and Socioeconomic Predictors of Normalizing IPV",
#'            pred_prob = TRUE,
#'            source_info = ", AmericasBarometer 2021",
#'            ymin = -0.3,
#'            ymax = 0.2)
#'}
#'@export
#'@import ggplot2
#'@import ggtext
#'@import showtext
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu}
#'

lapop_coef <- function(data, coef_var = data$coef, label_var = data$proplabel,
                       varlabel_var = data$varlabel, lb = data$lb, ub = data$ub,
                       pval_var = data$pvalue,
                       lang = "en",
                       main_title = "",
                       subtitle = "",
                       source_info = "",
                       sort = "coef",
                       order = "hi-lo",
                       ymin = NULL,
                       ymax = NULL,
                       pred_prob = FALSE,
                       color_scheme = "#784885",
                       dot_size = 5.5,
                       coef_label_size = 5,
                       subtitle_h_just = 0){

  plot_data <- data.frame(
    coef_var = coef_var,
    label_var = label_var,
    varlabel_var = varlabel_var,
    lb = lb,
    ub = ub,
    pval_var = pval_var,
    stringsAsFactors = FALSE
  )

  if(sort == "coef"){
    plot_data <- plot_data[order(-plot_data$coef_var), ]
  } else if(sort == "alpha"){
    plot_data <- plot_data[order(plot_data$varlabel_var), ]
  }

  if(order == "lo-hi"){
    plot_data <- plot_data[nrow(plot_data):1, ]
  }

  plot_data$varlabel_var = factor(plot_data$varlabel_var, levels = rev(unique(plot_data$varlabel_var)))
  coef_var = plot_data$coef_var
  label_var = plot_data$label_var
  varlabel_var = plot_data$varlabel_var
  lb = plot_data$lb
  ub = plot_data$ub
  pval_var = plot_data$pval_var

  sig = ifelse(pval_var < 0.05, FALSE, TRUE)
  label_x = ub
  label_hjust = -0.15
  subtitle_text = ifelse(
    subtitle != "",
    subtitle,
    ifelse(
      lang == "es",
      ifelse(pred_prob == TRUE, "Probabilidades pronosticadas", "Coeficientes de regresi\u00f3n"),
      ifelse(pred_prob == TRUE, "Predicted probabilities", "Regression coefficients")
    )
  )
  ci_text = ifelse(lang == "es",
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                   paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                          "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                          "<span style='color:#585860'>interval</span>"))
  update_geom_defaults("text", list(family = "inter"))# roboto
  ggplot(plot_data, aes(x = varlabel_var, y = coef_var)) +
    geom_hline(
      yintercept = 0,
      color = "#b8b8bf",
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_errorbar(aes(x=varlabel_var, ymin = lb, ymax = ub), width = 0.3, lty = 1, color = color_scheme) +
    geom_point(aes(x = varlabel_var, y = coef_var, fill = sig), color = "black", size = dot_size, shape = 21) +
    geom_text(aes(y = label_x, label = label_var, hjust = label_hjust),
              size = coef_label_size, color = color_scheme, fontface = "bold") +
    scale_fill_manual(values = color_scheme,
                      labels = paste0(" <span style='color:#585860; font-size:13pt'> ",
                                      subtitle_text,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                      ci_text),
                      limits = "FALSE",
                      na.value = "white") +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(ymin, ymax),
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    labs(title = main_title,
         y = " ",
         x = " ",
         caption = paste0(ifelse(lang == "es", "Fuente: LAPOP Lab", "Source: LAPOP Lab"),
                          source_info)) +
    theme(text = element_text(size = 14, family = "inter"), # roboto
          plot.title = element_text(size = 18, family = "inter", face = "bold"), # nunito
          plot.caption = element_text(size = 10.5, hjust = 0, vjust = 2, family = "inter", color="#585860"), # nunito
          plot.subtitle = element_text(size = 14, family = "inter-light", color="#585860"), # nunito-light
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 14, family = "inter", color = "#585860"), # roboto
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.margin = margin(t = 10, r = 30, b = 5.5, l = 5.5),
          legend.justification='left',
          #legend.margin = margin(t=0, b=0),
          legend.text = element_markdown(family = "inter-light"), # nunito-light
          legend.key=element_blank(),
          legend.margin=margin(0, 0, 0, -30-subtitle_h_just))
  }


