#' StatTeacherAssistant: An App that Assists Intro Statistics Instructors with
#'   Data Sets
#'
#' @description Includes an interactive application designed to support
#'   educators in wide-ranging disciplines, with a particular focus on those
#'   teaching introductory statistical methods (descriptive and/or inferential)
#'   for data analysis. Users are able to randomly generate data, make new
#'   versions of existing data through common adjustments (e.g., add random
#'   normal noise and perform transformations), and check the suitability of the
#'   resulting data for statistical analyses.
#'
#' @aliases StatTeacherAssistant-package StatTeacherAssistant
#'
#' @import shiny
#' @importFrom DescTools BinomCI BinomDiffCI LeveneTest PostHocTest RoundTo
#' @importFrom dplyr arrange bind_cols filter group_by na_if relocate rename
#'   select summarize tally
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom ggplot2 aes aes_string element_blank geom_abline geom_bar
#'   geom_boxplot geom_histogram geom_point ggplot labs stat_qq stat_qq_line
#'   theme theme_grey theme_set ylab
#' @importFrom plotly hide_legend layout plot_ly plotlyOutput renderPlotly style
#' @importFrom rhandsontable hot_to_r renderRHandsontable rhandsontable
#'   rHandsontableOutput
#' @importFrom rio export import
#' @importFrom rmatio read.mat write.mat
#' @importFrom shinyalert shinyalert
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs hide show useShinyjs
#' @importFrom sortable rank_list
#' @importFrom stats addmargins aov chisq.test cor IQR lm median na.omit pnorm
#'   quantile rbinom rmultinom rnorm runif sd t.test
#' @importFrom stringi stri_dup
#' @importFrom stringr str_count str_detect str_trim
#' @importFrom teachingApps rbeta4
#' @importFrom tidyr drop_na all_of
#' @importFrom utils capture.output head read.csv
#'
#' @section Function: \itemize{
#' \item \code{\link{runStatTeacherAssistantApp}}
#' }
#'
#' @details Package: StatTeacherAssistant \cr
#' Type: Package \cr
#' Version: 0.0.1 \cr
#' Date: 2022-11-23 \cr
#' Depends: R (>= 3.5.0) \cr
#' Imports: DescTools, dplyr, DT, ggplot2, plotly, rhandsontable, rio, rmatio,
#' shiny, shinyalert, shinyBS, shinyjs, sortable, stringi, stringr,
#' teachingApps, tidyr
#' License: MIT \cr
#' BugReports: https://github.com/ccasement/StatTeacherAssistant/issues \cr
#' Encoding: UTF-8 \cr
#'
#' @author
#' Christopher Casement \cr
#' Department of Mathematics \cr
#' Fairfield University \cr
#' \email{casementc@@gmail.com}
#'
#' Laura McSweeney \cr
#' Department of Mathematics \cr
#' Fairfield University
#'
#' @docType package
"_PACKAGE"
