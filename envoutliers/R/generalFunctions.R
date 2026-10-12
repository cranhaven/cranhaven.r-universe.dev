#' Kernel regression smoothing
#'
#' Nonparametric estimation of regression function using kernel regression with local or global data-adaptive plug-in bandwidth and optimal kernels.
#'
#' @param x data values.
#' Supported data types
#' \itemize{
#'   \item{a numeric vector}
#'   \item{a time series object \code{ts}}
#'   \item{a time series object \code{xts}}
#'   \item{a time series object \code{zoo}}
#' }
#' @param y a numeric vector of data values.
#' @param bandwidth.type a character string specifying the type of bandwidth.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"local"}} {(default) to use local bandwidth}
#'   \item{\code{"global"}} {to use global bandwidth}
#' }
#' @param bandwidth.value a local bandwidth array (for \code{bandwidth.type = "local"}) or global bandwidth value (for \code{bandwidth.type = "global"}) for kernel regression estimation. If \code{bandwidth.type = "NULL"} (default), a data-adaptive local plug-in (Herrmann, 1997) (for \code{bandwidth.type = "local"}) or data-adaptive global plug-in (Gasser et al., 1991) (for \code{bandwidth.type = "global"}) bandwidth is used instead.
#' @param kernel.order a nonnegative integer giving the order of the optimal kernel (Gasser et al., 1985) used for smoothing.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{kernel.order = 2}} {(default)}
#'   \item{\code{kernel.order = 4}}
#' }
#' @details This function computes the estimate of kernel regression function using a local or global data-adaptive plug-in algorithm and optimal kernels (Gasser et al., 1985).
#' @return A list is returned with elements:
#' \item{data.smoothed}{a numeric vector of estimates of the kernel regression function (smoothed data).}
#' \item{residuals}{a numeric vector of smoothing residuals}
#' @references Gasser T, Kneip A, Kohler W (1991). A flexible and fast method for automatic smoothing. Journal of the American Statistical Association, 86, 643-652.
#'
#' Herrmann E (1997). Local bandwidth choice in kernel regression estimation. Journal of Computational and Graphical Statistics, 6(1), 35-54.
#'
#' Gasser, T, Müller, H-G, Mammitzsch, V (1985). Kernels for nonparametric curve estimation. Journal of the Royal Statistical Society, B Met., 47(2), 238-252.
#'
#' Eva Herrmann; Packaged for R and enhanced by Martin Maechler (2016). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-8. https://CRAN.R-project.org/package=lokern
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' smoothed = smoothing(y = x)
#' smoothed$data.smoothed
#' smoothed$residuals
#' @importFrom lokern glkerns lokerns
#' @export
smoothing <- function(x = c(1:length(y)),
                      y,
                      bandwidth.type = "local",
                      bandwidth.value = NULL,
                      kernel.order = 2) {
  # input data check
  if (any(c("ts", "zoo", "xts") %in% class(y))) {
    y = as.numeric(y)
  }

  if (!kernel.order %in% c(2, 4)) {
    stop("Invalid kernel order")
  }

  data.input = data.frame(x, y)
  data = na.omit(data.input)
  data$smoothed = NA

  if (bandwidth.type == "local" & is.null(bandwidth.value)) {
    data$smoothed = lokerns(x = data$x, y = data$y, x.out = data$x, deriv = 0, kernel.order)$est
  } else if (bandwidth.type == "local" & !is.null(bandwidth.value)) {
    data$smoothed = lokerns(x = data$x, y = data$y, x.out = data$x, deriv = 0, bandwidth = bandwidth.value, kernel.order)$est
  } else if (bandwidth.type == "global" & is.null(bandwidth.value)) {
    data$smoothed = glkerns(x = data$x, y = data$y, x.out = data$x, deriv = 0, kernel.order)$est
  } else if (bandwidth.type == "global" & !is.null(bandwidth.value)) {
    data$smoothed = glkerns(x = data$x, y = data$y, x.out = data$x, deriv = 0, bandwidth = bandwidth.value, kernel.order)$est
  } else {
    stop("Invalid bandwidth type or value")
  }

  data.output = merge(data.input, data, by = c("x", "y"), all.x = TRUE)
  data.output = data.output[order(data.output$x),]

  return = list(data.smoothed = data.output$smoothed,
                residuals = data.output$y - data.output$smoothed)
}

#' Changepoint outlier detection plot - Only intended for developer use
#'
#' Plot of results obtained using function \code{\link{KRDetect.outliers.changepoint}} for identification of outliers using changepoint analysis.
#' The function is called by \code{\link{plot.KRDetect}} and is not intended for use by regular users of the package.
#'
#' @param x a list obtained as an output of function \code{\link{KRDetect.outliers.changepoint}} for identification of outliers using changepoint analysis.
#' @param show.segments a logical variable specifying if vertical lines representing individual segments are plotted.
#' @param ... further arguments to be passed to the \code{\link{plot}} function.
#' @details This function plots the results obtained using function \code{\link{KRDetect.outliers.changepoint}} identificating outliers using changepoint analysis based method.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for plotting results obtained using functions implemented in package \pkg{envoutliers}.
#' @importFrom graphics plot points segments
changepoint.plot <- function(x,
                             show.segments,
                            ...) {
  plot(x$index, x$x, ...)
  points(x$index, x$smoothed, type = "l", col = "darkgrey")
  points(x$index[x$outlier], x$x[x$outlier], col = "red", pch = 16)

  if (show.segments) {
    for (i in 1:max(x$changepoints, na.rm = TRUE)) {
      segments(min(x$index[x$changepoints == i], na.rm = TRUE), min(x$x, na.rm = TRUE),  min(x$index[x$changepoints == i], na.rm = TRUE), max(x$x, na.rm = TRUE), lty = 2 )
    }
  }
}

#' Control chart outliers detection plot - Only intended for developer use
#'
#' Plot of results obtained using function \code{\link{KRDetect.outliers.controlchart}} for identification of outliers using control charts.
#' The function is called by \code{\link{plot.KRDetect}} and is not intended for use by regular users of the package.
#'
#' @param x a list obtained as an output of function \code{\link{KRDetect.outliers.controlchart}} for identification of outliers using control charts.
#' @param plot.type a type of plot with outliers displayed.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"all"}} {to show outliers detected using control chart \emph{x}, \emph{R} and \emph{s}}
#'   \item{\code{"x"}} {to show outliers detected using control chart \emph{x}}
#'   \item{\code{"R"}} {to show outliers detected using control chart \emph{R}}
#'   \item{\code{"s"}} {to show outliers detected using control chart \emph{s}}
#' }
#' @param ... further arguments to be passed to the \code{\link{plot}} function.
#' @details This function plots the results obtained using function \code{\link{KRDetect.outliers.controlchart}} identificating outliers using control charts.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for plotting results obtained using functions implemented in package \pkg{envoutliers}.
#' @importFrom graphics close.screen par plot points screen split.screen
controlchart.plot <- function(x,
                              plot.type = "all",
                              ...) {
  if (plot.type == "all") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier], x$x[x$outlier], col = "red", pch = 16)
  } else if (plot.type == "x") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier.x], x$x[x$outlier.x], col = "red", pch = 16)
  } else if (plot.type == "R") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier.R], x$x[x$outlier.R], col = "red", pch = 16)
  } else if (plot.type == "s") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier.s], x$x[x$outlier.s], col = "red", pch = 16)
  } else {
    stop ("Invalid plot type")
  }
}

#' Extreme value outlier detection plot - Only intended for developer use
#'
#' Plot of results obtained using function \code{\link{KRDetect.outliers.EV}} for identification of outliers using extreme value theory.
#' The function is called by \code{\link{plot.KRDetect}} and is not intended for use by regular users of the package.
#'
#' @param x a list obtained as an output of function \code{\link{KRDetect.outliers.EV}} for identification of outliers using extreme value theory.
#' @param plot.type a type of plot with outliers displayed.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"all"}} {to show outliers with both extremely low and high value}
#'   \item{\code{"min"}} {to show outliers with extremely low value}
#'   \item{\code{"max"}} {to show outliers with extremely high value}
#' }
#' @param ... further arguments to be passed to the \code{\link{plot}} function.
#' @details This function plots the results obtained using function \code{\link{KRDetect.outliers.EV}} identificating outliers using changepoint analysis based method.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for plotting results obtained using functions implemented in package \pkg{envoutliers}.
#' @importFrom graphics par plot points screen split.screen
EV.plot <- function(x,
                    plot.type = "all",
                    ...) {
  if (plot.type == "all") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier], x$x[x$outlier], col = "red", pch = 16)
  } else if (plot.type == "min") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier.min], x$x[x$outlier.min], col = "red", pch = 16)
  } else if (plot.type == "max") {
    plot(x$index, x$x, ...)
    points(x$index, x$smoothed, type = "l", col = "darkgrey")
    points(x$index[x$outlier.max], x$x[x$outlier.max], col = "red", pch = 16)
  } else {
    stop ("Invalid plot type")
  }
}

#' Outlier detection plot
#'
#' Plot of results obtained using functions \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} and \code{\link{KRDetect.outliers.EV}} for identification of outliers.
#' The function graphically visualizes results obtained using functions for outlier detection implemented in package \pkg{envoutliers}.
#'
#' @param x a KRDetect object obtained as an output of function \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} or \code{\link{KRDetect.outliers.EV}} for identification of outliers.
#' @param show.segments a logical variable specifying if vertical lines representing individual segments are plotted. Only required for results obtained using \code{\link{KRDetect.outliers.changepoint}} function.
#' @param plot.type a type of plot with outliers displayed.
#'
#' Possible options for \code{\link{KRDetect.outliers.controlchart}} are
#' \itemize{
#'   \item{\code{"all"}} {to show outliers detected using control chart \emph{x}, \emph{R} and \emph{s}}
#'   \item{\code{"x"}} {to show outliers detected using control chart \emph{x}}
#'   \item{\code{"R"}} {to show outliers detected using control chart \emph{R}}
#'   \item{\code{"s"}} {to show outliers detected using control chart \emph{s}}
#' }
#'
#' Possible options for \code{\link{KRDetect.outliers.EV}} are
#' \itemize{
#'   \item{\code{"all"}} {to show outliers with both extremely low and high value}
#'   \item{\code{"min"}} {to show outliers with extremely low value}
#'   \item{\code{"max"}} {to show outliers with extremely high value}
#' }
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param ... further arguments to be passed to the \code{\link{plot}} function.
#' @details This function plots the results obtained using function \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} or \code{\link{KRDetect.outliers.EV}} implemented in package \pkg{envoutliers} and identificating outliers.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' result = KRDetect.outliers.EV(x)
#' plot(result)
#' @export
plot.KRDetect <- function(x,
                          show.segments = TRUE,
                          plot.type = "all",
                          xlab = "index",
                          ylab = "data values",
                          ...) {
  if (x$method.type == "changepoint analysis") {
    changepoint.plot(x, show.segments = show.segments, xlab = xlab, ylab = ylab, ...)
  } else if (x$method.type == "control chart") {
    controlchart.plot(x, plot.type = plot.type, xlab = xlab, ylab = ylab, ...)
  } else if (x$method.type == "extreme value theory") {
    EV.plot(x, plot.type = plot.type, xlab = xlab, ylab = ylab, ...)
  } else {
    stop("Invalid method")
  }
}

#' Summary of the outlier detection results
#'
#' Summary of results obtained using functions \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} and \code{\link{KRDetect.outliers.EV}} for identification of outliers.
#'
#' @param object a KRDetect object obtained as an output of function \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} or \code{\link{KRDetect.outliers.EV}} for identification of outliers.
#' @param ... further arguments to be passed to the \code{\link{summary}} function.
#' @details The function summarizes the results obtained using functions \code{\link{KRDetect.outliers.changepoint}}, \code{\link{KRDetect.outliers.controlchart}} and \code{\link{KRDetect.outliers.EV}} for identification of outliers.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' result = KRDetect.outliers.changepoint(x)
#' summary(result)
#' result = KRDetect.outliers.controlchart(x)
#' summary(result)
#' result = KRDetect.outliers.EV(x)
#' summary(result)
#' @export
summary.KRDetect <- function(object,
                             ...) {
  cat(paste0("Outlier detection"), "\n\n")
  cat(paste0("Method type: ", object$method.type), "\n")
  cat(paste0("Data length (incl. missing values): ", length(object$x)), "\n")
  outliers.ratio = length(which(object$outlier == TRUE)) / length(object$x)
  cat(paste0("Total number of detected outliers: ", length(which(object$outlier == TRUE)), " (", round(outliers.ratio * 100, 5), " %)"), "\n")

  if (object$method.type == "changepoint analysis") {
    cat(paste0("Total number of detected changepoints: ", max(object$changepoints, na.rm = TRUE) - 1), "\n")
    cat(paste0("Normality of residuals: ", object$normality.results$normality.residuals , "\n"))
    if (!is.null(object$normality.results$normality.residuals.transformed)) {
      cat(paste0("Normality of transformed residuals: ", object$normality.results$normality.residuals.transformed, "\n"))
    } else {
      cat(paste0("Normality of transformed residuals: NULL\n"))
    }
    cat(paste0("Outlier residuals detection method: ", object$detection.method))
  } else if (object$method.type == "control chart") {
    outliers.x.ratio = length(which(object$outlier.x == TRUE)) / length(object$x)
    cat(paste0("Total number of detected outliers based on x chart: ", length(which(object$outlier.x == TRUE)), " (", round(outliers.x.ratio * 100, 5), " %)"), "\n")
    outliers.R.ratio = length(which(object$outlier.R == TRUE)) / length(object$x)
    cat(paste0("Total number of detected outliers based on R chart: ", length(which(object$outlier.R == TRUE)), " (", round(outliers.R.ratio * 100, 5), " %)"), "\n")
    outliers.s.ratio = length(which(object$outlier.s == TRUE)) / length(object$x)
    cat(paste0("Total number of detected outliers based on s chart: ", length(which(object$outlier.s == TRUE)), " (", round(outliers.s.ratio * 100, 5), " %)"), "\n")
  } else if (object$method.type == "extreme value theory") {
    outliers.min.ratio = length(which(object$outlier.min == TRUE)) / length(object$x)
    cat(paste0("Total number of detected outliers with extremely low value: ", length(which(object$outlier.min == TRUE)), " (", round(outliers.min.ratio * 100, 5), " %)"), "\n")
    outliers.max.ratio = length(which(object$outlier.max == TRUE)) / length(object$x)
    cat(paste0("Total number of detected outliers with extremely high value: ", length(which(object$outlier.max == TRUE)), " (", round(outliers.max.ratio * 100, 5), " %)"), "\n")
  }
}
