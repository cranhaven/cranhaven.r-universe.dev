#' Plot a Long Data Frame
#'
#' This function is used to plot data from class 'LongData' as it is returned by \code{\link{getData}} \code{\link{simulate.GPPM}}.
#'
#' @param x longitudinal data frame of class 'LongData'.
#'
#' @param plotIds vector of IDs for which the data should be printed. Can be left empty. Then 5 IDs are picked randomly.
#'
#' @param by label of the variable on the x-axis. Can be left empty.
#'
#' @param ID label of the ID column. Can be left empty.
#'
#' @param DV label of the variable on the y-axis. Can be left empty.
#'
#' @param ... additional parameters (currently not used).
#'
#' @return a fitted Gaussian process panel model, which is an object of class 'GPPM'
#' @examples
#' data("demoLGCM")
#' plot(demoLGCM, plotIds = c(1, 2, 3))
#' plot(demoLGCM) # five random ids
#' @method plot LongData
#' @export
plot.LongData <- function(x, plotIds, by, ID, DV, ...) {
  if (!is.null(attr(x, "preds")) && missing(by)) {
    by <- attr(x, "preds")
  }
  x <- as_LongData(x, ID, DV)
  idCol <- attr(x, "ID")
  dvCol <- attr(x, "DV")
  if (missing(plotIds)) {
    Ids <- unique(x[, idCol])
    nIds <- length(Ids)
    plotIds <- sample(Ids, min(5, nIds))
  }
  plotData <- x[x[, idCol] %in% plotIds, ]
  plotData[, idCol] <- as.factor(plotData[, idCol])

  toPlot <- ggplot(plotData, aes(x = .data[[by]], y = .data[[dvCol]], colour = .data[[idCol]])) +
    ggplot2::geom_line() +
    ggthemes::theme_tufte()

  return(toPlot)
}
