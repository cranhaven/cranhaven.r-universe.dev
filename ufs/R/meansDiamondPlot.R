#' Diamond plots
#'
#' This function generates a so-called diamond plot: a plot based on the forest
#' plots that are commonplace in meta-analyses. The underlying idea is that
#' point estimates are uninformative, and it would be better to focus on
#' confidence intervals. The problem of the points with errorbars that are
#' commonly employed is that the focus the audience's attention on the upper
#' and lower bounds, even though those are the least relevant values. Using
#' diamonds remedies this.
#'
#'
#' @param data,dat The dataframe containing the variables (`items`) to show in
#' the diamond plot (the name `dat` for this argument is deprecated but still
#' works for backward compatibility).
#' @param items Optionally, the names (or numeric indices) of the variables
#' (items) to show in the diamond plot. If NULL, all columns (variables, items)
#' will be used.
#' @param labels A character vector of labels to use instead of column names
#' from the dataframe.
#' @param decreasing Whether to sort the variables (rows) in the diamond plot
#' decreasing (TRUE), increasing (FALSE), or not at all (NULL).
#' @param conf.level The confidence of the confidence intervals.
#' @param showData Whether to show the raw data or not.
#' @param dataAlpha This determines the alpha (transparency) of the data
#' points. Note that argument `alpha` can be used to set the alpha of the
#' diamonds; this is eventually passed on to [ufs::ggDiamondLayer()].
#' @param dataSize The size of the data points.
#' @param dataColor The color of the data points.
#' @param diamondColors A vector of the same length as there are rows in the
#' dataframe, to manually specify colors for the diamonds.
#' @param jitterWidth How much to jitter the individual datapoints
#' horizontally.
#' @param jitterHeight How much to jitter the individual datapoints vertically.
#' @param returnLayerOnly Set this to TRUE to only return the
#' [ggplot2::ggplot()] layer of the diamondplot, which can be useful to
#' include it in other plots.
#' @param xlab,ylab The labels of the X and Y axes.
#' @param theme The theme to use.
#' @param xbreaks Where the breaks (major grid lines, ticks, and labels) on the
#' x axis should be.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see `ggsaveParams`).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Additional arguments are passed to [diamondPlot()]
#' and eventually to [ggDiamondLayer()]. This can be used to, for
#' example, specify two or more colors to use to generate a gradient (using
#' `generateColors` and maybe `fullColorRange`).
#' @return A [ggplot2::ggplot()] plot with a [ggDiamondLayer()] is
#' returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso See [diamondPlot()], [meanSDtoDiamondPlot()], [ggDiamondLayer()], [factorLoadingDiamondCIplot()]
#' @keywords hplot
#' @examples
#'
#' tmpDf <- data.frame(item1 = rnorm(50, 1.6, 1),
#'                     item2 = rnorm(50, 2.6, 2),
#'                     item3 = rnorm(50, 4.1, 3));
#'
#' ### A simple diamond plot
#' meansDiamondPlot(tmpDf);
#'
#' ### A diamond plot with manually
#' ### specified labels and colors
#' meansDiamondPlot(tmpDf,
#'                  labels=c('First',
#'                           'Second',
#'                           'Third'),
#'                   diamondColors=c('blue', 'magenta', 'yellow'));
#'
#' ### Using a gradient for the colors
#' meansDiamondPlot(tmpDf,
#'                  labels=c('First',
#'                           'Second',
#'                           'Third'),
#'                  generateColors = c("magenta", "cyan"),
#'                  fullColorRange = c(1,5));
#'
#' @export meansDiamondPlot
meansDiamondPlot <- function(data, items = NULL, labels = NULL,
                             decreasing=NULL,
                             conf.level=.95,
                             showData = TRUE, dataAlpha = .1, dataSize=3,
                             dataColor = "#444444",
                             diamondColors = NULL,
                             jitterWidth = .5,
                             jitterHeight = .4,
                             returnLayerOnly = FALSE,
                             xlab='Scores and means',
                             ylab=NULL,
                             theme=ggplot2::theme_bw(),
                             xbreaks = "auto",
                             outputFile = NULL,
                             outputWidth = 10,
                             outputHeight = 10,
                             ggsaveParams = ufs::opts$get("ggsaveParams"),
                             dat=NULL,
                             ...) {

  if (missing(data)) {
    if (!is.null(dat)) {
      data <- dat;
    }
  }

  dat <- data;

  res <- list();
  res$intermediate <- list();

  if (is.null(items)) items <- names(dat);

  res$intermediate$dat <- ufs::varsToDiamondPlotDf(dat, items = items,
                                                   labels = labels,
                                                   decreasing=decreasing,
                                                   conf.level=conf.level);

  ### Get labels from this dataframe, because they may have been sorted
  labels <- res$intermediate$dat$label;

  diamondLayer <-
    ufs::diamondPlot(res$intermediate$dat, ciCols=c('lo', 'mean', 'hi'),
                     yLabels = labels, colorCol=diamondColors,
                     returnLayerOnly = TRUE, ...);

  if (returnLayerOnly) {
    return(diamondLayer);
  }

  plot <- ggplot2::ggplot();

  if (showData) {
    plot <- plot +
      ufs::rawDataDiamondLayer(dat, items=items,
                               itemOrder = res$intermediate$dat$rownr,
                               dataAlpha=dataAlpha,
                               dataColor=dataColor,
                               jitterWidth = jitterWidth,
                               jitterHeight = jitterHeight,
                               size=dataSize);
  }

  plot <- plot + diamondLayer +
    ggplot2::scale_y_continuous(breaks=sort(res$intermediate$dat$rownr),
                                minor_breaks=NULL,
                                labels=res$intermediate$dat$label) +
    theme +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank());

  if (!is.null(xbreaks) &&
      length(xbreaks) == 1 &&
      tolower(xbreaks) == "auto") {
    ### If we only have a few values, set these as xbreaks. If we have
    ### more than 10, don't set any breaks manually
    potentialBreaks <- sort(unique(unlist(dat[, items])));
    if (length(potentialBreaks) <= 10) {
      plot <- plot + ggplot2::scale_x_continuous(breaks=potentialBreaks);
    }
  } else if (is.numeric(xbreaks)) {
    plot <- plot + ggplot2::scale_x_continuous(breaks=xbreaks, labels=xbreaks);
  }

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggplot2::ggsave, ggsaveParameters);
  }

  attr(plot, 'itemOrder') <- res$intermediate$dat$rownr;

  return(plot);
}

