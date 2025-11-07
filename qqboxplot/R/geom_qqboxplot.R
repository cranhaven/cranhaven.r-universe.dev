#' A modification of the boxplot with information about the tails
#'
#' @section Description:
#' The Q-Q boxplot inherits its summary statistics from the boxplot.  See
#' [geom_boxplot()] for details.  The Q-Q boxplot differs from the boxplot
#' by using more informative whiskers than the regular boxplot.
#'
#' The vertical position of the whiskers can be interpreted as it is in the
#' boxplot, and the maximal vertical value is chosen as it is done in the
#' regular boxplot.  The horizontal positioning of the whiskers indicates the
#' deviation of the data set of interest from some reference data set
#' (specified as either a theoretical distribution or an actual data set).
#' Taking the central vertical axis of the boxplot as being zero, deviations
#' to the right indicate that those values are larger than the
#' corresponding data points in the reference data set, where two data points
#' correspond if their quantiles match.  Deviations to the left indicate that
#' the values are smaller than their corresponding data points.  Consider a
#' situation where your data set has fatter tails than the normal distribution.
#' When the reference distribution is the normal distribution, then the whiskers
#' below the box will be left of the central axis (the left tail values are
#' smaller than they ought to be) and the whiskers above the box will be right
#' of the central axis (the right tail values are larger than the ought to be).
#'
#' In order to compare the data set of interest to the reference data set, they
#' must be on the same scale.  The Q-Q boxplot uses Tukey's g-h distribution
#' to determine the appropriate scaling factor.
#'
#' Much of the code here is a modification of the geom_boxplot() code.
#'
#' @inheritParams ggplot2::geom_boxplot
#' @param stat specifies the stat function to use
#'
#' @return Returns an object of class `GeomQqboxplot`, (inherits from `Geom`, `ggproto`),
#'  that renders the data for the Q-Q boxplot.
#'
#' @export
#' @examples
#' p <- ggplot2::ggplot(simulated_data, ggplot2::aes(factor(group,
#' levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16",
#' "t distribution, df=8", "t distribution, df=4")), y=y))
#' p + geom_qqboxplot()
#' p + geom_qqboxplot(reference_dist = "norm")
#'
#' \donttest{
#' p + geom_qqboxplot(compdata = comparison_dataset)
#' }
#'
#' # geom_qqboxplot inherits all arguments from geom_boxplot, e.g.:
#' p + geom_qqboxplot(notch = TRUE)
#' p + geom_qqboxplot(varwidth=TRUE)
#' p + geom_qqboxplot(ggplot2::aes(color = group)) + ggplot2::guides(color=FALSE)
#'
geom_qqboxplot <- function(mapping = NULL, data = NULL,
                           stat = "qqboxplot", position = "dodge2",
                           ...,
                           outlier.colour = NULL,
                           outlier.color = NULL,
                           outlier.fill = NULL,
                           outlier.shape = 19,
                           outlier.size = 1.5,
                           outlier.stroke = 0.5,
                           outlier.alpha = NULL,
                           notch = FALSE,
                           notchwidth = 0.5,
                           varwidth = FALSE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- ggplot2::position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warning("Can't preserve total widths when varwidth = TRUE.", call. = FALSE)
      position$preserve <- "single"
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQqboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}


GeomQqboxplot <- ggplot2::ggproto("GeomQqboxplot", ggplot2::Geom,

                         # need to declare `width`` here in case this geom is used with a stat that
                         # doesn't have a `width` parameter (e.g., `stat_identity`).
                         extra_params = c("na.rm", "width"),

                         setup_data = function(data, params) {
                           data$width <- data$width %||%
                             params$width %||% (resolution(data$x, FALSE) * 0.9)

                           if (!is.null(data$outliers)) {
                             suppressWarnings({
                               out_min <- vapply(data$outliers, min, numeric(1))
                               out_max <- vapply(data$outliers, max, numeric(1))
                             })

                             data$ymin_final <- pmin(out_min, data$ymin)
                             data$ymax_final <- pmax(out_max, data$ymax)
                           }

                           # if `varwidth` not requested or not available, don't use it
                           if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
                             data$xmin <- data$x - data$width / 2
                             data$xmax <- data$x + data$width / 2
                           } else {
                             # make `relvarwidth` relative to the size of the largest group
                             data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
                             data$xmin <- data$x - data$relvarwidth * data$width / 2
                             data$xmax <- data$x + data$relvarwidth * data$width / 2
                           }
                           data$width <- NULL
                           if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL

                           data
                         },

                         draw_group = function(data, panel_params, coord, fatten = 2,
                                               outlier.colour = NULL, outlier.fill = NULL,
                                               outlier.shape = 19,
                                               outlier.size = 1.5, outlier.stroke = 0.5,
                                               outlier.alpha = NULL,
                                               notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {

                           common <- base::data.frame(
                             colour = data$colour,
                             size = data$size,
                             linetype = data$linetype,
                             fill = scales::alpha(data$fill, data$alpha),
                             group = data$group,
                             stringsAsFactors = FALSE
                           )


                           whiskers <- base::data.frame(
                             x = as.integer(data$x)+c(data$deviatlower[[1]], data$deviatupper[[1]]),
                             y = c(data$lowery[[1]], data$uppery[[1]]),
                             alpha = NA,
                             common,
                             stringsAsFactors = FALSE
                           )

                           whisker_upper_bound <- base::data.frame(
                             x = c(as.integer(data$x)+data$upperlower[[1]], rev(as.integer(data$x)+data$upperupper[[1]])),
                             y = c(data$uppery[[1]], rev(data$uppery[[1]])),
                             alpha = .1,
                             fill=scales::muted('red'),
                             colour=NA,
                             common,
                             stringsAsFactors = FALSE
                           )

                           whisker_lower_bound <- base::data.frame(
                             x = c(as.integer(data$x)+data$lowerlower[[1]], rev(as.integer(data$x)+data$lowerupper[[1]])),
                             y = c(data$lowery[[1]], rev(data$lowery[[1]])),
                             alpha = .1,
                             fill=scales::muted('red'),
                             colour=NA,
                             common,
                             stringsAsFactors = FALSE
                           )

                           whisker_upper_bound_point <- base::data.frame(
                             x = c(as.integer(data$x)+data$upperlower[[1]], rev(as.integer(data$x)+data$upperupper[[1]])),
                             y = c(data$uppery[[1]], rev(data$uppery[[1]])),
                             alpha = .15,
                             fill=NA,
                             colour=scales::muted('red'),
                             size=.5,
                             shape = outlier.shape %||% data$shape[1],
                             stroke = outlier.stroke %||% data$stroke[1],
                             stringsAsFactors = FALSE
                           )

                           whisker_lower_bound_point <- base::data.frame(
                             x = c(as.integer(data$x)+data$lowerlower[[1]], rev(as.integer(data$x)+data$lowerupper[[1]])),
                             y = c(data$lowery[[1]], rev(data$lowery[[1]])),
                             alpha = .15,
                             fill=NA,
                             colour=scales::muted('red'),
                             size=.5,
                             shape = outlier.shape %||% data$shape[1],
                             stroke = outlier.stroke %||% data$stroke[1],
                             stringsAsFactors = FALSE
                           )




                           box <- base::data.frame(
                             xmin = data$xmin,
                             xmax = data$xmax,
                             ymin = data$lower,
                             y = data$middle,
                             ymax = data$upper,
                             ynotchlower = ifelse(notch, data$notchlower, NA),
                             ynotchupper = ifelse(notch, data$notchupper, NA),
                             notchwidth = notchwidth,
                             alpha = data$alpha,
                             common,
                             stringsAsFactors = FALSE
                           )

                           if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                             outliers <- base::data.frame(
                               y = data$outliers[[1]],
                               x = data$x[1],
                               colour = outlier.colour %||% data$colour[1],
                               fill = outlier.fill %||% data$fill[1],
                               shape = outlier.shape %||% data$shape[1],
                               size = outlier.size %||% data$size[1],
                               stroke = outlier.stroke %||% data$stroke[1],
                               fill = NA,
                               alpha = outlier.alpha %||% data$alpha[1],
                               stringsAsFactors = FALSE
                             )
                             outliers_grob <- ggplot2::GeomPoint$draw_panel(outliers, panel_params, coord)
                           } else {
                             outliers_grob <- NULL
                           }

                           ggname("geom_qqboxplot", grid::grobTree(
                             outliers_grob,
                             ggplot2::GeomPath$draw_panel(whiskers, panel_params, coord),
                             ggplot2::GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord),
                             ggplot2::GeomPolygon$draw_panel(whisker_upper_bound, panel_params, coord),
                             ggplot2::GeomPolygon$draw_panel(whisker_lower_bound, panel_params, coord),
                             ggplot2::GeomPoint$draw_panel(whisker_upper_bound_point, panel_params, coord),
                             ggplot2::GeomPoint$draw_panel(whisker_lower_bound_point, panel_params, coord)
                           ))
                         },

                         draw_key = ggplot2::draw_key_boxplot,

                         default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                           alpha = NA, shape = 19, linetype = "solid"),

                         required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax", "lowery")
)
