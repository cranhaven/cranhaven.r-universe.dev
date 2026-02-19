#' Plot of parameter estimates with bootstrap confidence intervals
#'
#' @param bootEstimates Bootstrap estimates (for example, shape or scale)
#'
#' @inheritParams PlotRL
#'
#' @return Plot of parameter estimates.
#' @seealso \code{\link{SplineML}} and \code{\link{LocalEstim}} for examples.
#' @examples
#' ## See examples in vignettes:
#' # vignette("localMethods", package = "circularEV")
#' # vignette("splineML", package = "circularEV")
#' @export
#' @import ggplot2
PlotParamEstim <- function(bootEstimates, thetaGrid=1:360, alpha=0.05,
                           ylim=NULL, cex.axis=15, cex.lab=2, thrWidth=2, ylab=NULL, thrColor="#D45E1A"){

  z <- stats::qnorm(1-alpha/2)

  meanEst <- apply(bootEstimates, 1, mean, na.rm=T)
  xiMLE_sd <- apply(bootEstimates, 1, stats::sd, na.rm=T)
  confLow <- meanEst - z*xiMLE_sd
  confUp <- meanEst + z*xiMLE_sd
  df <- data.frame(meanEst, confLow, confUp, thetaGrid=thetaGrid)

  if(is.null(ylim)){
    ylim <- c(min(confLow), max(confUp))
  }

  p <- ggplot(data = df, mapping = aes(thetaGrid, meanEst)) +
    theme(plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim=ylim)
  p <- p + geom_line() + geom_ribbon(mapping = aes(ymin=confLow, ymax=confUp), alpha=0.2) +
    labs(x=bquote(theta), y=ylab) +
    theme(axis.title.y = element_text(size = rel(cex.lab), angle = 90)) +
    theme(axis.title.x = element_text(size = rel(cex.lab), angle = 0)) +
    theme(plot.title = element_text(lineheight=2, face="bold", color="black", size=15)) +
    theme(axis.text.x = element_text(size=cex.axis)) +
    theme(axis.text.y = element_text(size=cex.axis))

  return(p)

}
