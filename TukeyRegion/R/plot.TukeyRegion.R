plot.TukeyRegion <- function(x, newPlot = TRUE, drawPoints = TRUE, 
                             drawRidges = TRUE, 
                             colorBackground = "white", 
                             colorPoints = "red", 
                             colorFacets = "blue", 
                             colorRidges = "green", 
                             lwd = 1, lty = 1, alpha = 1, ...){
  TukeyRegionPlot(x, newPlot, drawPoints, drawRidges, 
                  colorBackground, colorPoints, colorFacets, colorRidges, 
                  lwd2D = lwd, lty2D = lty, alpha)
}
