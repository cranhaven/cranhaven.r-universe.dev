#' Polar plot of circular data
#'
#' @param Data Response variable
#' @param drc Directional covariate
#' @param thr Threshold values along thetaGrid
#' @param ylim Range of values
#' @param pointSize Size of points (observations)
#' @param fontSize Font size
#' @param thrWidth Threshold width
#' @param thrColor Threshold colour
#'
#' @return Polar plot of circular data, possibly including a threshold
#' @examples
#' data(HsSP)
#' data(drc)
#'
#' PolarPlotData(Data=HsSP, drc=drc, thr=NULL, pointSize=4, fontSize=14,
#'               thrWidth=4, ylim=c(0,max(HsSP)))
#'
#' data(thresholdExampleML) # loads threshold example
#'
#' PolarPlotData(Data=HsSP, drc=drc, thr=thresholdExampleML, pointSize=4,
#'               fontSize=12, thrWidth=4, ylim=c(0,max(HsSP)))
#' @export
# @importFrom plotly plot_ly
# @importFrom plotly '%>%'
# @importFrom plotly add_trace
# @importFrom plotly layout
# @importFrom plotly hide_legend
PolarPlotData <- function(Data, drc, thr=NULL, ylim=NULL, pointSize=1, fontSize=12,
                          thrWidth=4, thrColor="#D45E1A"){

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

  if(is.null(ylim)){
    ylim <- range(Data)
    ylim[2] <- ylim[2]*1.03
  }
  df <- data.frame(angle = drc,
                   frequency = Data)
  polar <- plot_ly(data=df, type = 'scatterpolar', mode = "markers",
                   width = 500, height = 500) %>%
    add_trace(theta=~angle, r=~frequency,
              marker = list(
                size = pointSize,
                color = '#000000',
                opacity = 1
              )
    )
  polar <- polar %>% layout(
    polar = list(radialaxis = list(angle = 90, range = ylim),
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
  if(!is.null(thr)){
    thetaVec <- seq(0, 360, length.out = length(thr))
    polar <- polar  %>% add_trace(theta=thetaVec,
                                  r=thr,
                                  mode = "lines",
                                  line=list(width=thrWidth, color=thrColor))
  }

  polar <- hide_legend(polar)
  return(polar)
}
