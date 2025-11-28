#' ### Note: this is necessary to prevent Rcmd CHECK from throwing a note;
#' ### otherwise it think these variable weren't defined yet.
#' # utils::globalVariables(c('distribution', '..density..',
#' #                          '..scaled', 'normalX', 'normalY'));
#'
#' ### Make a 'better' histogram using ggplot
#'
#'
#' #' normalHist
#' #'
#' #' normalHist generates a histogram with a density curve and a normal density
#' #' curve.
#' #'
#' #'
#' #' @param vector A numeric vector.
#' #' @param histColor The colour to use for the histogram.
#' #' @param distributionColor The colour to use for the density curve.
#' #' @param normalColor The colour to use for the normal curve.
#' #' @param distributionLineSize The line size to use for the distribution
#' #' density curve.
#' #' @param normalLineSize The line size to use for the normal curve.
#' #' @param histAlpha Alpha value ('opaqueness', as in, versus transparency) of
#' #' the histogram.
#' #' @param xLabel Label to use on x axis.
#' #' @param yLabel Label to use on y axis.
#' #' @param normalCurve Whether to display the normal curve.
#' #' @param distCurve Whether to display the curve showing the distribution of
#' #' the observed data.
#' #' @param breaks The number of breaks to use (this is equal to the number of
#' #' bins minus one, or in other words, to the number of bars minus one).
#' #' @param theme The theme to use.
#' #' @param rug Whether to add a rug (i.e. lines at the bottom that correspond to
#' #' individual datapoints.
#' #' @param jitteredRug Whether to jitter the rug (useful for variables with
#' #' several datapoints sharing the same value.
#' #' @param rugSides This is useful when the histogram will be rotated; for
#' #' example, this can be set to 'r' if the histogram is rotated 270 degrees.
#' #' @param rugAlpha Alpha value to use for the rug. When there is a lot of
#' #' overlap, this can help get an idea of the number of datapoints at 'popular'
#' #' values.
#' #' @param returnPlotOnly Whether to return the usual \code{normalHist} object
#' #' that also contains all settings and intermediate objects, or whether to only
#' #' return the [ggplot2::ggplot()] plot.
#' #' @param x The object to print.
#' #' @param ... Any additional arguments are passed to the default `print` method.
#' #' @rdname normalHist
#' #' @return
#' #'
#' #' An object, with the following elements: \item{input}{The input when the
#' #' function was called.} \item{intermediate}{The intermediate numbers and
#' #' distributions.} \item{dat}{The dataframe used to generate the plot.}
#' #' \item{plot}{The histogram.}
#' #' @author Gjalt-Jorn Peters
#' #'
#' #' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' #' @keywords utilities
#' #' @examples normalHist(mtcars$mpg)
#' #'
#' #' @export
#' normalHist <- function(vector,
#'                        histColor = "#0000CC",
#'                        distributionColor = "#0000CC",
#'                        normalColor = "#00CC00",
#'                        distributionLineSize = 1,
#'                        normalLineSize = 1,
#'                        histAlpha = .25,
#'                        xLabel = NULL,
#'                        yLabel = NULL,
#'                        normalCurve = TRUE,
#'                        distCurve = TRUE,
#'                        breaks = 30,
#'                        theme=ggplot2::theme_minimal(),
#'                        rug=NULL, jitteredRug=TRUE, rugSides="b",
#'                        rugAlpha = .2,
#'                        returnPlotOnly = FALSE) {
#'   varName <- deparse(substitute(vector));
#'   vector <- stats::na.omit(vector);
#'   if (!is.numeric(vector)) {
#'     tryCatch(vector <- as.numeric(vector), error = function(e) {
#'       stop("The vector you supplied is not numeric; I tried to convert it, ",
#'            "but my attempt failed. The error I got is:\n", e);
#'     });
#'   }
#'
#'   ### Create object to return, storing all input variables
#'   res <- list(input = as.list(environment()), intermediate = list(), output = list());
#'
#'   res$input$sampleSize = length(vector);
#'   res$intermediate$distribution <- res$input$vector;
#'
#'   ### Compute binwidth and scaling factor for density lines
#'   res$intermediate$tempBinWidth <- (max(res$input$vector) -
#'                                     min(res$input$vector)) / breaks;
#'   scalingFactor <- max(table(cut(vector, breaks=breaks)));
#'
#'   if (normalCurve) {
#'     res$intermediate$normalX <- c(seq(min(res$input$vector), max(res$input$vector),
#'                                       by=(max(res$input$vector) -
#'                                             min(res$input$vector)) /
#'                                         (res$input$sampleSize-1)));
#'     res$intermediate$normalY <- stats::dnorm(res$intermediate$normalX,
#'                                              mean=mean(res$input$vector),
#'                                              sd=stats::sd(res$input$vector));
#'     res$intermediate$normalY <- (1 / max(res$intermediate$normalY)) *
#'       scalingFactor *
#'       res$intermediate$normalY;
#'     res$dat <- data.frame(normalX = res$intermediate$normalX,
#'                           normalY = res$intermediate$normalY,
#'                           distribution = res$intermediate$distribution);
#'   } else {
#'     res$dat <- data.frame(distribution = res$intermediate$distribution);
#'   }
#'
#'   ### Generate labels if these weren't specified
#'   if (is.null(xLabel)) {
#'     xLabel <- paste0('Value of ', ufs::extractVarName(varName));
#'   }
#'   if (is.null(yLabel)) {
#'     yLabel <- "Frequency";
#'   }
#'
#'   ### Plot distribution
#'   res$plot <- ggplot2::ggplot(data=res$dat,
#'                               ggplot2::aes_string(x='distribution')) +
#'     ggplot2::xlab(xLabel) +
#'     ggplot2::ylab(yLabel) +
#'     ggplot2::geom_histogram(color=NA, fill=histColor, na.rm = TRUE,
#'                             alpha=histAlpha, binwidth=res$intermediate$tempBinWidth);
#'   if (distCurve) {
#'     res$plot <- res$plot +
#'       ggplot2::geom_line(ggplot2::aes_q(y=bquote(..scaled.. * .(scalingFactor))),
#'                          stat = 'density',
#'                          color=distributionColor, size=distributionLineSize,
#'                          na.rm = TRUE);
#'   }
#'   if (normalCurve) {
#'     res$plot <- res$plot +
#'       ggplot2::geom_line(ggplot2::aes_string(x='normalX', y='normalY'),
#'                          color=normalColor, size=normalLineSize,
#'                          na.rm = TRUE);
#'   }
#'   res$plot <- res$plot + theme;
#'
#'   if (is.null(rug)) {
#'     if (nrow(res$dat) < 1000) {
#'       rug <- TRUE;
#'     } else {
#'       rug <- FALSE;
#'     }
#'   }
#'
#'   if (rug) {
#'     if (jitteredRug) {
#'       res$plot <- res$plot +
#'         ggplot2::geom_rug(color=distributionColor, sides=rugSides,
#'                           ggplot2::aes(y=0), position="jitter",
#'                           alpha=rugAlpha,
#'                           na.rm = TRUE);
#'     } else {
#'       res$plot <- res$plot +
#'         ggplot2::geom_rug(color=distributionColor, sides=rugSides,
#'                           alpha=rugAlpha,
#'                           na.rm = TRUE);
#'     }
#'   }
#'
#'   if (!is.null(res$input$xLabel) && is.logical(res$input$xLabel) &&
#'       !(res$input$xLabel)) {
#'     res$plot <- res$plot +
#'       ggplot2::theme(axis.title.x = ggplot2::element_blank());
#'   }
#'   if (!is.null(res$input$yLabel) && is.logical(res$input$yLabel) &&
#'       !(res$input$yLabel)) {
#'     res$plot <- res$plot +
#'       ggplot2::theme(axis.title.y = ggplot2::element_blank());
#'   }
#'
#'   if (returnPlotOnly) {
#'     return(res$plot)
#'   } else {
#'     ### Set class and return result
#'     class(res) <- "normalHist";
#'     return(res);
#'   }
#' }
#'
#' #' @method print normalHist
#' #' @rdname normalHist
#' #' @export
#' print.normalHist <- function(x, ...) {
#'   print(x$plot, ...);
#' }
