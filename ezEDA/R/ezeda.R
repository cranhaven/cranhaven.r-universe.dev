#' ezeda: A package for task oriented exploratory data analysis
#'
#' The ezeda package provides functions for visualizations for exploratory
#' data analysis. Whereas graphic packages generally provide many functions
#' that users assemble to create suitable plots, each ezeda function warps
#' ggplot and other code to generate a complete plot for common exploratory
#' data analysis task corresponding to a recurring pattern.
#'
#' ezeda provides five categories of functions:
#' tally, contribution, measure distribution, measure relationship,
#' and measure trend
#'
#' @section tally functions:
#' \itemize{
#'   \item category_tally
#'   \item two_category_tally
#' }
#' @section contribution functions:
#' \itemize{
#'   \item category_contribution
#'   \item two_category_contribution
#' }
#' @section measure distribution functions:
#' \itemize{
#'   \item measure_distribution
#'   \item measure_distribution_by_category
#'   \item measure_distribution_by_two_categories
#'   \item measure_distribution_by_time
#' }
#' @section measure relationship functions:
#' \itemize{
#'   \item two_measures_relationship
#'   \item multi_measure_relationship
#' }
#' @section measure trend functions:
#' \itemize{
#'   \item measure_change_over_time
#'   \item measure_change_over_time_long
#' }
#' @docType package
#' @name ezeda
#' @importFrom GGally ggpairs
#' @importFrom rlang !! !!! :=
#' @importFrom stats reorder
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom scales comma
#' @importFrom magrittr %>%
NULL
#> NULL
