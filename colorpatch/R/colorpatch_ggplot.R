#' @include colorpatch_methods.R
NULL

#' A ggplot2 theme for rendering colorpatches (black background)
#'
#' @param fill background fill color (default: "black")
#' @param plot.background background fill color (default: "black")
#' @return a theme function for showing color patches
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(colorpatch)
#' dat <- CreateExampleData()
#' df <- ToDataFrame(dat)
#' p <- ggplot(df) + theme_colorpatch() + stat_colorpatch(aes(ratio=ratio,conf=conf,x=x,y=y))
theme_colorpatch <- function(fill = "black", plot.background = fill) {
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    legend.position = "none",
    legend.background = ggplot2::element_rect(fill = fill),
    panel.background = ggplot2::element_rect(fill = fill),
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = plot.background))
}

#' A [ggplot2::ggproto] class for showing color patches.
StatColorPatch <- ggplot2::ggproto(
  "StatColorPatch",
  ggplot2::Stat,
  required_aes = c("ratio", "conf", "x", "y"),
  setup_params = function(data, params) {
    if (is.null(params$color.fun)) {
      params$color.fun <- ColorPatchColorFun()
    }
    if (is.null(params$size.fun)) {
      params$size.fun <- ColorPatchSizeFun()
    }
    if (is.null(params$thresh.ratio)) {
      params$thresh.ratio <- 1
    }
    if (is.null(params$thresh.conf)) {
      params$thresh.conf <- 1
    }
    if (is.null(params$min.size)) {
      params$min.size <- 0.01
    }
    params
  },
  compute_group = function(data, scales, color.fun, size.fun,
                           thresh.ratio, thresh.conf, min.size) {
    n <- nrow(data)
    data$ratio <- data$ratio / thresh.ratio
    data$conf <- data$conf / thresh.conf
    data$ratio[data$ratio < -1] <- -1
    data$ratio[data$ratio > 1] <- 1
    data$conf[data$conf < min.size] <- min.size
    data$conf[data$conf > 1] <- 1
    sz <- unlist(mapply(size.fun, data$ratio, data$conf))
    col <- unlist(mapply(color.fun, data$ratio, data$conf))
    grid <- data.frame(
      x = data$x,
      y = data$y,
      fill = col,
      width = sz,
      height = sz)
    grid
  }
)

#' A stat function for the use with ggplot2
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'  If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. 
#'  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param geom Defaults to `tile`.
#' @param position Position adjustment, either as a string, or the 
#' result of a call to a position adjustment function.
#' @param na.rm  If `FALSE`, the default, missing values are removed with a warning. 
#' If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? 
#' `NA`, the default, includes if any aesthetics are mapped.
#' `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than 
#' combining with them. This is most useful for helper functions that define 
#' both data and aesthetics and shouldn't inherit behaviour from the default 
#' plot specification, e.g. borders.
#' @param color.fun Color function mapping a (ratio,conf) pair to a color (defaults to [colorpatch::ColorPatchColorFun()]).
#' @param size.fun Size function mapping a (ratio,conf) pair to a rectangle size (defaults to [colorpatch::ColorPatchSizeFun()] returning constantly 1).
#' @param ... Further arguments given to the [colorpatch::StatColorPatch] ggproto object. 
#' Here thresh.ratio, thresh.conf are the most important parameters.
#'
#' @return a ggplot statistics layer for showing color patches
#' @export
stat_colorpatch <- function(mapping = NULL,
                            data = NULL,
                            geom = "tile",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            color.fun = ColorPatchColorFun(),
                            size.fun = ColorPatchSizeFun(),
                            ...) {
  ggplot2::layer(
    stat = StatColorPatch,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  color.fun = color.fun,
                  size.fun = size.fun, ...)
  )
}

#' Plots a ratio/confidence plot using a bivariate colormap
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'  If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. 
#'  You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer.
#' @param geom Defaults to `tile`.
#' @param position Position adjustment, either as a string, or the 
#' result of a call to a position adjustment function.
#' @param na.rm  If `FALSE`, the default, missing values are removed with a warning. 
#' If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? 
#' `NA`, the default, includes if any aesthetics are mapped.
#' `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than 
#' combining with them. This is most useful for helper functions that define 
#' both data and aesthetics and shouldn't inherit behaviour from the default 
#' plot specification, e.g. borders.
#' @param color.fun Color function mapping a (ratio,conf) pair to a color (defaults to [colorpatch::HsvColorFun()]).
#' @param size.fun Size function mapping a (ratio,conf) pair to a rectangle size (defaults to [colorpatch::HsvSizeFun()] returning constantly 1).
#' @param ... further arguments given to the [StatColorPatch()] function
#'
#' @return a ggplot statistics layer for showing bicolored maps
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(colorpatch)
#' dat <- CreateExampleData()
#' df <- ToDataFrame(dat)
#' p <- ggplot(df) + theme_colorpatch() + stat_bicolor(aes(ratio=ratio,conf=conf,x=x,y=y))
stat_bicolor <- function(mapping = NULL,
                        data = NULL,
                        geom = "tile",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        color.fun = HsvColorFun(),
                        size.fun = HsvSizeFun(),
                        ...) {
  ggplot2::layer(
    stat = StatColorPatch,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  color.fun = color.fun,
                  size.fun = size.fun, ...)
  )
}
