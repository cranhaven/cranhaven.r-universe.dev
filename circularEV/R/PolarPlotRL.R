#' Polar plot of T-year levels
#'
#' @param pointSize Size of points (observations)
#' @param fontSize Font size
#' @param lineWidth Threshold width
#'
#' @inheritParams PlotRL
#'
#' @return Polar plot of T-year levels.
#' @seealso \code{\link{SplineML}} and \code{\link{LocalEstim}} for examples.
#' @examples
#' ## See also examples in vignettes:
#' # vignette("localMethods", package = "circularEV")
#' # vignette("splineML", package = "circularEV")
#' @export
# @importFrom plotly plot_ly
# @importFrom plotly '%>%'
# @importFrom plotly add_trace
# @importFrom plotly layout
# @importFrom plotly hide_legend
PolarPlotRL <- function(RLBootList, Data, drc, thetaGrid=1:360, TTs, whichPlot, alpha=0.05,
                        ylim=NULL, pointSize=4, fontSize=12, lineWidth=4){

  plot_ly <- NA
  "%>%" <- NA
  add_trace <- NA
  layout <- NA
  hide_legend <- NA
  if(requireNamespace("plotly", quietly=T)){
    plot_ly <- plotly::plot_ly
    "%>%" <- plotly::"%>%"
    add_trace <- plotly::add_trace
    layout <- plotly::layout
    hide_legend <- plotly::hide_legend
  }else{
    warning("Please install 'plotly' package for using
    'plot_ly', '%>%', 'add_trace', 'layout', and 'hide_legend'.")
  }

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
  df <- data.frame(angle = rep(thetaGrid, 3),
                   frequency = c(RL_df$RL_ConfLow,
                                 RL_df$RLBootMat_mean,
                                 RL_df$RL_ConfUp),
                   z = as.character(sort(rep(1:3, length(thetaGrid)))))
  df <- stats::aggregate(frequency ~ angle + z, data = df, sum)
  pal <- c("white", "darkgrey", "darkgrey")
  RLplot <- plot_ly(data=df, type='scatterpolar', mode="lines", colors=pal, width=500, height=500) %>%
    add_trace(theta=~angle, r=~frequency, fill="tonext", color=~z, line=list(width=0)) # or type="toself"
  RLplot <- RLplot %>% layout(
    polar = list(radialaxis = list(angle = 90,
                                   range = ylim
    ),
    angularaxis = list(
      rotation = 90,
      direction = 'clockwise',
      ticktext =  c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"),
      tickvals = seq(0, 360, 45)
    )
    ),
    margin = list(b = 30, l = 30, r = 30, t = 30, pad = 0, autoexpand = TRUE),
    font = list(size=fontSize)
  )
  RLplot <- hide_legend(RLplot)
  RLplot <- RLplot %>% add_trace(theta=thetaGrid,
                                 r=RL_df$RLBootMat_mean,
                                 # fill="tonext",
                                 line=list(width=lineWidth, color="black"))

  RLplot <- RLplot %>% add_trace(theta=drc, r=Data,
                                 # fill="tonext",
                                 mode="markers",
                                 marker=list(size=pointSize, color="black"))

  return(RLplot)
}
