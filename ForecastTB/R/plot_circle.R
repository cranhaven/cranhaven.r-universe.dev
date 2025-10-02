#' Function to plot comparison of Predicted values in a circular ring
#'
#' @param x as output object of 'prediction_errors()' function
#' @param ... arguments passed to or from other methods
#' @import circlize
#' @import RColorBrewer
#' @importFrom graphics segments
#' @importFrom graphics text
#' @return Returns error comparison plots for forecasting methods
#' @export
#' @examples
#' a <- prediction_errors(data = nottem)
#' plot_circle(a)

plot_circle <- function(x, ...)
{
  object <- x
  args <- list(...)
  res <- list()
  xx <- seq(0, 360, (360/object@parameters$nval))
  nn <- object@output$Predicted_Values
  nn <- nn[,1:object@parameters$nval]
  minv <- min(nn[1,])
  maxv <- max(nn[1,])

  for(i in 1:nrow(nn)){
    nn[i,] <- (nn[i,] - minv)/(maxv - minv)
  }

  cl <- brewer.pal(n = nrow(nn), name = "Spectral") #RdBu
  factors = letters[1]
  circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
  circos.initialize(factors = factors, xlim = c(0, object@parameters$nval))
  circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA, bg.col = "black", track.height = 0.04)
  circos.axis(sector.index = "a", major.at = 0:12, labels = "", direction = "inside", labels.cex = 1.3, major.tick.percentage = 0.5)
  circos.text(1:object@parameters$nval, rep(2, object@parameters$nval), 1:object@parameters$nval, facing = "downward", col = 'black', cex = 2)

  text(1.1, 1, 'Methods', cex = 2, font = 2, col = 'black')
  for(j in 1:nrow(nn)){
    segments(sin(xx[12]/180*pi)*nn[j,][object@parameters$nval], cos(xx[12]/180*pi)*nn[j,][object@parameters$nval], sin(xx[1]/180*pi)*nn[j,][1], cos(xx[1]/180*pi)*nn[j,][1], lwd=3, col = cl[j])
    text(1.1, (1-(j/10)), row.names(nn)[j], cex = 2, font = 2, col = cl[j])
    for(i in 1:object@parameters$nval){
      segments(sin(xx[i]/180*pi)*nn[j,][i], cos(xx[i]/180*pi)*nn[j,][i], sin(xx[i+1]/180*pi)*nn[j,][i+1], cos(xx[i+1]/180*pi)*nn[j,][i+1], lwd=3 , col = cl[j])
      segments(0,0, sin(xx[i]/180*pi)*0.95, cos(xx[i]/180*pi)*0.95, col = 'gray', lwd=1)
    }
  }
}

