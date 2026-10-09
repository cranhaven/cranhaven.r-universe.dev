#' @import methods
#' @import stats
#' @import Epoch
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom foreach foreach %dopar%
#' @importFrom progress progress_bar
#' @importFrom ramify pprint
#' @importFrom ggplot2 ggplot aes geom_line 
#' xlab ylab labs 
#' scale_x_discrete scale_y_discrete geom_tile scale_y_continuous
#' geom_ribbon scale_color_manual theme element_text  theme_minimal 
#' @importFrom ggtext element_markdown
#' @importFrom viridis scale_fill_viridis
#' @exportMethod plot
NULL

pkgData <- new.env()
pkgData$debug <- FALSE

debug <- function() {
    pkgData$debug <- TRUE
}

undebug <- function() {
    pkgData$debug <- FALSE
}
