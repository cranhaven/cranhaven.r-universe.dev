#' Basic diamond plot construction function
#'
#' This function constructs a diamond plot using [ggDiamondLayer()].
#' It's normally not necessary to call this function directly: instead, use
#' [meansDiamondPlot()] [meanSDtoDiamondPlot()], and
#' [factorLoadingDiamondCIplot()].
#'
#'
#' @param data A dataframe (or matrix) containing lower bounds, centers (e.g.
#' means), and upper bounds of intervals (e.g. confidence intervals).
#' @param ciCols The columns in the dataframe with the lower bounds, centers
#' (e.g. means), and upper bounds (in that order).
#' @param colorCol The column in the dataframe containing the colors for each
#' diamond, or a vector with colors (with as many elements as the dataframe has
#' rows).
#' @param otherAxisCol The column in the dataframe containing the values that
#' determine where on the Y axis the diamond should be placed. If this is not
#' available in the dataframe, specify it manually using `yValues`.
#' @param yValues The values that determine where on the Y axis the diamond
#' should be placed (can also be a column in the dataframe; in that case, use
#' `otherAxisCol`.
#' @param yLabels The labels to use for for each diamond (placed on the Y
#' axis).
#' @param xlab,ylab The labels of the X and Y axes.
#' @param autoSize Whether to make the height of each diamond conditional upon
#' its length (the width of the confidence interval).
#' @param fixedSize If not using relative heights, `fixedSize` determines
#' the height to use.
#' @param theme The theme to use.
#' @param color Color to use if colors are specified for each diamond.
#' @param returnLayerOnly Set this to TRUE to only return the
#' [ggplot2::ggplot()] layer of the diamondplot, which can be useful to
#' include it in other plots.
#' @param outputFile A file to which to save the plot.
#' @param outputWidth,outputHeight Width and height of saved plot (specified in
#' centimeters by default, see `ggsaveParams`).
#' @param ggsaveParams Parameters to pass to ggsave when saving the plot.
#' @param \dots Additional arguments will be passed to
#' [ggDiamondLayer()].
#' @return A [ggplot2::ggplot()] plot with a [ggDiamondLayer()] is
#' returned.
#' @author Gjalt-Jorn Peters
#'
#' Maintainer: Gjalt-Jorn Peters <ufs@@opens.science>
#' @seealso [meansDiamondPlot()], [meanSDtoDiamondPlot()],
#' [ggDiamondLayer()], [factorLoadingDiamondCIplot()]
#' @keywords hplot
#' @examples
#'
#' tmpDf <- data.frame(lo = c(1, 2, 3),
#'                     mean = c(1.5, 3, 5),
#'                     hi = c(2, 4, 10),
#'                     color = c('green', 'red', 'blue'));
#'
#' ### A simple diamond plot
#' diamondPlot(tmpDf);
#'
#' ### A diamond plot using the specified colours
#' diamondPlot(tmpDf, colorCol = 4);
#'
#' ### A diamond plot using automatically generated colours
#' ### using a gradient
#' diamondPlot(tmpDf, generateColors=c('green', 'red'));
#'
#' ### A diamond plot using automatically generated colours
#' ### using a gradient, specifying the minimum and maximum
#' ### possible values that can be attained
#' diamondPlot(tmpDf, generateColors=c('green', 'red'),
#'             fullColorRange=c(1, 10));
#'
#'
#' @export diamondPlot
diamondPlot <- function(data,
                        ciCols=1:3,
                        colorCol=NULL,
                        otherAxisCol=NULL,
                        yValues=NULL, yLabels=NULL, ylab = NULL,
                        autoSize=NULL, fixedSize=.15,
                        xlab='Effect Size Estimate',
                        theme=ggplot2::theme_bw(), color='black',
                        returnLayerOnly = FALSE,
                        outputFile = NULL,
                        outputWidth = 10,
                        outputHeight = 10,
                        ggsaveParams = ufs::opts$get("ggsaveParams"),
                        ...) {

  ### In case we want to check for a complete dataframe
  # if (sum(complete.cases(data[, c(ciCols, colorCol, otherAxisCol)])) < nrow(data)) {
  #   warning("The dataframe passed in argument 'data' contained rows with missing values! I am removing these rows.");
  #   data <- data[complete.cases(data[, c(ciCols, colorCol, otherAxisCol)]), ];
  # }

  if (!is.null(yValues)) {
    ### Check whether yValues specifies a column in 'data' or whether it's a vector
    if (length(yValues) == 1) {
      ### Probably index in dataframe
      if (is.character(yValues) && (yValues %in% names(data))) {
        yValues <- data[, yValues];
      } else if (yValues <= ncol(data)) {
        ### Consider it an index
        yValues <- data[, yValues]
      }
      ### Otherwise, we don't consider it an index (but as a vector of length one),
      ### so we keep it as is, just like when it /is/ a vector (of length > one)
    }
  }

  if (is.null(yValues)) {
    yValues <- 1:nrow(data);
  }

  ### Check whether yLabels specifies a column in 'data' or whether it's a vector
  if (!is.null(yLabels)) {
    if ((nrow(data) == 1) && (length(yLabels) == 1) && (is.character(yLabels))) {
      ### Don't do anything
    } else if (length(yLabels) == 1 && (nrow(data) > 1)) {
      ### Probably index in dataframe
      if (is.character(yLabels) && (yLabels %in% names(data))) {
        yLabels <- data[, yLabels];
      } else if (yLabels <= ncol(data)) {
        ### Consider it an index
        yLabels <- data[, yLabels];
      }
      ### Otherwise, we don't consider it an index (but as a vector of length one),
      ### so we keep it as is, just like when it /is/ a vector (of length > one)
    }
  } else if (inherits(yValues, 'character')) {
    yLabels <- yValues;
  } else if (!is.null(rownames(data))) {
    yLabels <- rownames(data);
  } else {
    yLabels <- yValues;
  }

  data$diamondPlotYLabelColumn <- diamondPlotYLabelColumn <- yLabels;

  if (length(colorCol) > 1) {
    if (length(colorCol) != nrow(data)) {
      stop("When specifying a vector as colorCol, this has ",
           "to be the same length as the dataframe!");
    }
    data$colorCol <- colorCol;
    colorCol <- 'colorCol';
  }

  if (is.null(otherAxisCol)) {
    data$otherAxisCol <- as.numeric(factor(yValues));
    otherAxisCol <- 'otherAxisCol';
  }

  diamondLayer <- ufs::ggDiamondLayer(data, ciCols = ciCols,
                                      colorCol = colorCol,
                                      otherAxisCol = otherAxisCol,
                                      autoSize=autoSize,
                                      fixedSize = fixedSize,
                                      color=color, ...);

  plot <- ggplot2::ggplot() +
    diamondLayer +
    ggplot2::scale_y_continuous(breaks=data$otherAxisCol, minor_breaks=NULL,
                               labels=diamondPlotYLabelColumn) +
    theme +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank());

  if (!is.null(outputFile)) {
    ggsaveParameters <- c(list(filename = outputFile,
                               plot = plot,
                               width = outputWidth,
                               height = outputHeight),
                          ggsaveParams);
    do.call(ggplot2::ggsave, ggsaveParameters);
  }

  if (returnLayerOnly) {
    return(diamondLayer);
  } else {
    return(plot);
  }

}
