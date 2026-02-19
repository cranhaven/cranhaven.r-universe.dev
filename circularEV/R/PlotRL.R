
#' Plot of T-year levels
#'
#' @param RLBootList List containing bootstrap estimates of T-year levels
#' @param TTs  T-year levels. For example, TTs = c(100, 10000).
#' @param whichPlot Index identifying which T-year level should be plotted from
#'   TTs. If TTs = c(100, 10000), then whichPlot=2 produces a plot for the
#'   10000-year level
#' @param alpha Significance level for the confidence intervals. Default to 0.05.
#' @param ylim Range for the y-axis
#' @param pointSize Size of points (observations)
#' @param cex.axis Graphical parameter
#' @param cex.lab Graphical parameter
#' @param thrWidth Threshold width
#' @param thrColor Threshold colour
#' @param ylab y-axis label
#'
#' @inheritParams ThrSelection
#'
#' @return Plot of T-year levels.
#' @seealso \code{\link{SplineML}} and \code{\link{LocalEstim}} for examples.
#' @examples
#' ## See also examples in vignettes:
#' # vignette("localMethods", package = "circularEV")
#' # vignette("splineML", package = "circularEV")
#' @export
#'
PlotRL <- function(RLBootList, Data, drc, thetaGrid=1:360, TTs, whichPlot, alpha=0.05,
                   ylim=NULL, pointSize=1, cex.axis=15, cex.lab=2, thrWidth=2,
                   thrColor="#D45E1A", ylab=NULL){

  z <- stats::qnorm(1-alpha/2)

  means <- apply(simplify2array(RLBootList), 1:2, mean)
  sds <- apply(simplify2array(RLBootList), 1:2, stats::sd)

  RLBootMat_mean <- means[,whichPlot]
  RLBootMat_sd <- sds[,whichPlot]
  RL_ConfLow <- RLBootMat_mean - z*RLBootMat_sd
  RL_ConfUp <- RLBootMat_mean + z*RLBootMat_sd
  RL_df <- data.frame(RLBootMat_mean, RL_ConfLow, RL_ConfUp, thetaGrid=thetaGrid)

  if(is.null(ylim)){
    ylim <- c(0, max(RL_ConfUp))
  }

  RL <- ggplot(data = RL_df, mapping = aes(thetaGrid, RLBootMat_mean)) +
    theme(plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim=ylim)
  RL <- RL + geom_line() + geom_ribbon(mapping = aes(ymin=RL_ConfLow, ymax=RL_ConfUp), alpha=0.2) +
    labs(x=bquote(theta), y=ylab) +
    theme(axis.title.y = element_text(size = rel(cex.lab), angle = 90)) +
    theme(axis.title.x = element_text(size = rel(cex.lab), angle = 0)) +
    theme(plot.title = element_text(lineheight=2, face="bold", color="black", size=15)) +
    theme(axis.text.x = element_text(size=cex.axis)) +
    theme(axis.text.y = element_text(size=cex.axis))

  df <- data.frame(drc, Data)
  RL <- RL +  geom_point(data = df, mapping = aes(drc, Data), size=pointSize) +
    theme(plot.title = element_text(hjust = 0.5))

  return(RL)

}
