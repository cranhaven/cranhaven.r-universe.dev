#' Histogram plot for an interval variable
#'
#' @param x An symbolic data table.
#' @param n.bins Numbers of breaks of the histogram.
#' @param ... Arguments to be passed to the barplot method.
#'
#' @return A list with componets : frequency and histogram
#'
#' @examples
#' data(oils)
#' res <- interval.histogram.plot(x = oils[, 3], n.bins = 3)
#' res
#' @export
#' @importFrom graphics barplot
interval.histogram.plot <- function(x, n.bins, ...) {
  x <- to.v2(x)
  if (x$M > 1) {
    stop("x must be a sym.data.table with only one variable of type interval.")
  }
  if (x$sym.var.types %in% c("$I")) {
    data. <- x$data
    data.min <- min(data.)
    data.max <- max(data.)
    n.intervals <- length(n.bins) - 1
    if (n.intervals == 0) {
      n.bins <- seq(from = data.min, to = data.max, length.out = n.bins + 1)
      n.bins <- round(x = n.bins, digits = 2)
    } else {
      int.min <- min(n.bins)
      int.max <- max(n.bins)
      if (data.min < int.min) {
        n.bins <- c(data.min, n.bins)
      }
      if (int.max < data.max) {
        n.bins <- c(n.bins, data.max)
      }
    }
    m <- nrow(data.)
    n.intervals <- length(n.bins) - 1

    data.i <- data.[, 1]
    data.s <- data.[, 2]

    freqs <- numeric(length = n.intervals)
    names <- character(length = n.intervals)

    for (i in 1:n.intervals) {
      interval.i <- n.bins[i]
      interval.s <- n.bins[i + 1]
      names[i] <- paste0("[", interval.i, ",", interval.s, "]")

      ind <- (data.s > interval.i) & (data.i < interval.s)
      x.i <- y.i <- data.i[ind]
      x.s <- y.s <- data.s[ind]
      y.i[x.i <= interval.i] <- interval.i
      y.s[interval.s <= x.s] <- interval.s

      freqs[i] <- 100 * sum((y.s - y.i) / (x.s - x.i)) / m
    }

    freqs <- round(x = freqs, digits = 1)

    histogram <- barplot(height = freqs, names.arg = names, ...)

    return(list(frequency = freqs, histogram = histogram))
  } else {
    stop("var.number does not point to an interval value variable")
  }
}
