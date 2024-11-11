#' @rdname survplot
#' @export
#' @useDynLib icrf
#'
#' @title 'Plotting individual survival curves'
#'
#' @description Plotting individual survival curves.
#'
#' @param x an object of class \code{icrf} or a survival matrix with rows
#' representing subjects and columns representing times
#' @param i subject index
#' @param smooth which curve of an \code{icrf} object to be plotted? smoothed or non-smoothed.
#' Ignored when \code{x} is a matrix.
#' @param timepoints A numeric vector. needed when the time attribute is missing.
#' @param title Title of the plot.
#' @param suppress.inf.time Do not draw the curve at \code{timepoints} being infinity?
#' @param ... 'Other graphical parameters to be passed on to' \code{ggplot}.
#'
#' @return 'Invisibly,' the vector of survival probabilities that are plotted.
#'
#'
#' @examples
#' # rats data example.
#' # Note that this is a toy example. Use a larger ntree and nfold in practice.
#' data(rat2)
#' \donttest{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=10, nfold=3)
#'  survplot(rats.icrf, c(1,3,5))
#' }
#' \dontshow{
#'  set.seed(1)
#'  rats.icrf <-
#'    icrf(~ dose.lvl + weight + male + cage.no, data = rat2,
#'         data.type = "currentstatus", currentstatus.label = c("survtime", "tumor"),
#'         returnBest = TRUE, ntree=2, nfold=2)
#'  survplot(rats.icrf, c(1,3,5))
#' }
#'
#' @author Hunyong Cho, Nicholas P. Jewell, and Michael R. Kosorok.
#'
#' @references
#' \href{https://arxiv.org/abs/1912.09983}{Cho H., Jewell N. J., and Kosorok M. R. (2020+). "Interval censored
#'  recursive forests"}
#'
survplot <- function(x, i, smooth = TRUE, timepoints = NULL,
                     title = "Estimated survival curve",
                     suppress.inf.time = TRUE, ...) {
  if (!inherits(x, "icrf")) {
    if (class(x)[1] != "matrix") {
      stop("This function only works for objects of class `icrf' or a matrix")
    }
  } else {
    x <- if(smooth) x$predicted.Sm else x$predicted
  }
  if (is.null(timepoints)) timepoints <- attr(x, "time")
  if (is.null(timepoints)) stop("Either the time attribute is missing in x or timepoints is not supplied.")
  if (length(timepoints) != dim(x)[2]) stop("Dimension of timepoints (or the time attribute in x) and the survival matrix do not match.")
  if (any(is.na(timepoints))) stop("There is a NA values in timepoints (or the time attribute in x).")
  if (any(!is.numeric(timepoints))) stop("timepoints (or the time attribute in x) should be numeric.")
  if (!any(i %in% 1:dim(x)[1])) stop("subject indicator (i) is not feasible.")

  if (suppress.inf.time) {
    inf.indicator <- is.infinite(timepoints)
    timepoints <- timepoints[!inf.indicator]
    x <- x[, !inf.indicator, drop = FALSE]
  }

  len.i = length(i)
  len.t = length(timepoints)
  p <-
    ggplot2::ggplot(data.frame(time = rep(timepoints, len.i),
                               s = as.numeric(t(x[i, ])),
                               subject = as.factor(rep(i, each = len.t)))) +
    ggplot2::geom_line(ggplot2::aes_string("time", "s", col = "subject", linetype = "subject")) +
    ggplot2::ylab("survival probability") +
    ggplot2::theme_bw() +
    ggplot2::ylim(0:1) +
    ggplot2::ggtitle(title)
  p
}
