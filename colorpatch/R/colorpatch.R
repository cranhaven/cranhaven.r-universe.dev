#' A small introduction to the [colorpatch] package.
#'
#' The colorpatch package provides functions for plotting **color patch grids** rendering
#' the two channels fold change and confidence value within a single diagram.
#' This is especially useful for analyzing gene expression data as well as other types of "change" data
#' such as gains/losses in stock exchange or analyzing the agricultural output.
#' 
#' The packages consists of:
#' - ggplot extensions for visualizing color patch grids [colorpatch::stat_colorpatch()] and [colorpatch::stat_bicolor()]
#' - Functionality for rearranging data for a better readable map [colorpatch::OrderData()]
#' - Perceptual optimization functions for sub-sampling non-uniform bicolored palettes [colorpatch::OptimizeBiColor()]
#' 
#' For more details see the vignette 
#' @examples 
#' vignette("introduction", package = "colorpatch")
"_PACKAGE"
