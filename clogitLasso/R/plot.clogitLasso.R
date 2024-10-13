#' Plot coefficients from a \code{clogitLasso} object
#'
#' Plot the parameter profile associated \code{clogitLasso} object
#'
#' @param x an objet of type \code{clogitLasso}
#' @param logLambda Set to  TRUE if the horizontal axis is on log scale
#' @param add.legend Take the value TRUE if legend should be printed in top right hand corner
#' @param add.labels set to TRUE if labels are to be added to curves at leftmost side
#' @param lty Same to \code{lty} parameter of plot function
#' @param col Same to \code{col} parameter of plot function
#' @param ... additional arguments to plot function
#' @author Marta Avalos, Helene Pouyes, Marius Kwemou and Binbin Xu
#' @references Avalos, M., Pouyes, H., Grandvalet, Y., Orriols, L., & Lagarde, E. (2015). \emph{Sparse conditional logistic
#'  regression for analyzing large-scale matched data from epidemiological studies: a simple algorithm.} BMC bioinformatics, 16(6), S1.  \doi{10.1186/1471-2105-16-S6-S1}.
#' @importFrom graphics matplot legend text
#' @examples
#' \dontrun{
#'  # generate data
#'  y <- rep(c(1,0), 100)
#'  X <- matrix (rnorm(20000, 0, 1), ncol = 100) # pure noise
#'  strata <- sort(rep(1:100, 2))
#'
#'  fitLasso <- clogitLasso(X,y,strata,log=TRUE)
#'  # plot
#'  plot(fitLasso)
#' }
#' @export
plot.clogitLasso <- function (x,
                             logLambda = TRUE,
                             add.legend = FALSE,
                             add.labels = TRUE,
                             lty = 1:ncol(x$beta),
                             col = 1:ncol(x$beta),
                             ...)
{
  if (logLambda)
    horiz <- log(x$fraction)
  else
    horiz <- x$fraction
  matplot(
    x = horiz,
    y = x$beta,
    type = "l",
    xlab = "Regularisation parameter",
    ylab = "Parameter estimate",
    lty  = lty,
    col  = col,
    ...
  )
  if (add.legend) {
    if (is.null(dimnames(x$x_rec))) {
      var.names <- paste("Variable", 1:ncol(x$beta))
    }
    else {
      var.names <- dimnames(x$x_rec)[[2]]
    }
    legend("topright",
           legend = var.names,
           lty = lty,
           col = col,
           ...)
  }
  if (add.labels) {
    if (is.null(dimnames(x$x_rec))) {
      plot.names <- 1:ncol(x$beta)
    }
    else {
      plot.names <- dimnames(x$x_rec)[[2]]
    }
    text(
      x = min(horiz),
      y = x$beta[nrow(x$beta),],
      labels = plot.names,
      lty = lty,
      col = col,
      ...
    )
  }
}