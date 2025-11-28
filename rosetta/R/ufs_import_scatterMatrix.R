#' Scatter Matrix
#'
#' scatterMatrix produces a matrix with jittered scatterplots, histograms, and
#' correlation coefficients.
#'
#' @param dat A dataframe containing the items in the scale. All variables in
#' this dataframe will be used if items is NULL.
#' @param items If not NULL, this should be a character vector with the names
#' of the variables in the dataframe that represent items in the scale.
#' @param itemLabels Optionally, labels to use for the items (optionally, named,
#' with the names corresponding to the `items`; otherwise, the order of the
#' labels has to match the order of the items)
#' @param plotSize Size of the final plot in millimeters.
#' @param sizeMultiplier Allows more flexible control over the size of the plot
#' elements
#' @param pointSize Size of the points in the scatterplots
#' @param axisLabels Passed to ggpairs function to set axisLabels.
#' @param normalHist Whether to use the default ggpairs histogram on the
#' diagonal of the scattermatrix, or whether to use the [ufs::normalHist()] version.
#' @param progress Whether to show a progress bar; set to `FALSE` to disable. See
#' [GGally::ggpairs()] help for more information.
#' @param theme The ggplot2 theme to use.
#' @param hideGrid Whether to hide the gridlines in the plot.
#' @param conf.level The confidence level of confidence intervals
#' @param ...  Additional arguments for `scatterMatrix()` are passed on to
#' [ufs::normalHist()], and additional arguments for the `print` method are passed
#' on to the default `print` method.
#' @return
#'
#' An object with the input and several output variables. Most notably:
#' \item{output$scatterMatrix}{A scattermatrix with histograms on the diagonal
#' and correlation coefficients in the upper right half.}
#' @keywords utilities univar
#' @rdname scatterMatrix
#' @examples
#'
#' ### Note: the 'not run' is simply because running takes a lot of time,
#' ###       but these examples are all safe to run!
#' \dontrun{
#'
#' ### Generate a datafile to use
#' exampleData <- data.frame(item1=rnorm(100));
#' exampleData$item2 <- exampleData$item1+rnorm(100);
#' exampleData$item3 <- exampleData$item1+rnorm(100);
#' exampleData$item4 <- exampleData$item2+rnorm(100);
#' exampleData$item5 <- exampleData$item2+rnorm(100);
#'
#' ### Use all items
#' scatterMatrix(dat=exampleData);
#' }
#'
#' @export
scatterMatrix <- ufs::scatterMatrix;
