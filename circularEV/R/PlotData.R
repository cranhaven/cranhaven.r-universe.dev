#' Plot of circular data
#'
#' @param Data Response variable
#' @param drc Directional covariate
#' @param thr Threshold values along thetaGrid
#' @param ylim Range of values
#' @param pointSize Size of points (observations)
#' @param cex.axis Graphical parameter
#' @param cex.lab Graphical parameter
#' @param thrWidth Threshold width
#' @param thrColor Threshold colour
#' @param thrLineType Graphical parameter
#' @param ylab y-axis label
#'
#' @return Plot of circular data, possibly including a threshold.
#' @examples
#' data(HsSP)
#' data(drc)
#'
#' PlotData(Data=HsSP, drc=drc, thr=NULL, pointSize=1, cex.axis=15, cex.lab=2,
#'          thrWidth=2)
#'
#' data(thresholdExampleML) # loads threshold example
#'
#' PlotData(Data=HsSP, drc=drc, thr=thresholdExampleML, pointSize=1, cex.axis=15,
#'          cex.lab=2, thrWidth=2)
#' @export
#' @import ggplot2
PlotData <- function(Data, drc, thr=NULL, ylim=NULL, pointSize=4,
                     cex.axis=15, cex.lab=2, thrWidth=2, thrColor="#D45E1A",
                     thrLineType=1, ylab=NULL){

  if(is.null(ylim)){
    ylim <- range(Data)
    ylim[2] <- ylim[2]*1.03
  }

  df <- data.frame(drc, Data)
  p <- ggplot(data = df, mapping = aes(drc, Data)) +
    theme(plot.title = element_text(hjust = 0.5)) + geom_point(size=pointSize) +
    coord_cartesian(ylim=ylim)

  if(!is.null(thr)){

    thr <- as.matrix(thr)
    nThr <- ncol(thr)
    if(length(thrColor)==1){
      thrColor <- rep(thrColor, nThr)
    }
    if(length(thrLineType)==1){
      thrLineType <- rep(thrLineType, nThr)
    }

    thetaVec <- seq(0, 360, length.out = length(thr)/nThr)

    for(i in 1:nThr){
      df_curves <- data.frame(thetaVec=thetaVec, thr=thr[,i])
      p <- p + geom_line(data=df_curves, aes(x=thetaVec, y=thr), size=thrWidth,
                         color=thrColor[i], linetype=thrLineType[i])
    }

  }

  p <- p + labs(x=bquote(theta), y=ylab) +
    theme(axis.title.y = element_text(size = rel(cex.lab), angle = 90)) +
    theme(axis.title.x = element_text(size = rel(cex.lab), angle = 0)) +
    theme(plot.title = element_text(lineheight=2, face="bold", color="black", size=15)) +
    theme(axis.text.x = element_text(size=cex.axis)) +
    theme(axis.text.y = element_text(size=cex.axis))
  return(p)

}

