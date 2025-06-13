
##############
# Functions for plotting variograms

plot.vg <- function(x, CTPM = NULL, col="black", col.CTPM = "red", fraction = 1, ...){
  
  SVF <- new.variogram(x)
  
  SVF@info$axes <- x@info$axes
  
  attr(SVF,"info")$lags <- attr(x,"info")$lags
  
  suppressWarnings(plot(SVF,
                        CTMM = CTPM,
                        col.CTMM = col.CTPM,
                        fraction = fraction,
                        ...))
}
