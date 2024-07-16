#' @title Binned representation of a time series
#' 
#' @description Groups large vectors into several bins of a given length for better (and faster) data plotting
#' @aliases binTimeSeries
#' @usage binTimeSeries(f, acqTime, nIntervals, mode = "mean", plot = TRUE)
#' @param f Numeric vector
#' @param acqTime Point acquisition rate (in seconds)
#' @param nIntervals Number of intervals into which the vector will be grouped
#' @param mode Set to "mean" (default) or "sum" to average or sum all the points in every interval, respectively
#' @param plot Boolean, set to TRUE (default) to plot the result
#' @details This function groups all the points in the vector 'f' into 'nIntervals' bins of length = length(f)/nIntervals.
#' Then, averages or sums all of the points in each bin and plots the result. 
#' 
#' @export
#' @return A data frame with two variables (Counts and Time) and 'nIntervals' observations
#' @author Alejandro Linares
#' 
#' @seealso \code{\link{binMatrix}}
#' 
#' @examples
#' \donttest{
#' ### Please navigate to
#' ### (https://github.com/FCSlib/FCSlib/tree/master/Sample%20Data)
#' ### to find this sample data
#' 
#' # Automatic plot
#' x <- readFileTiff("Cy5.tif")
#' x <- as.vector(x)
#' x <- binTimeSeries(x[length(x):1], 2e-6, 100, mode = "mean", plot = T)
#' 
#' # Manual plot (useful for adding custom labels)
#' x <- readFileTiff("Cy5.tif")
#' x <- as.vector(x)
#' x <- binTimeSeries(x[length(x):1], 2e-6, 100, mode = "mean", plot = F)
#' plot(x$Counts~x$Time, type = "l")
#' }

binTimeSeries <- function(f, acqTime, nIntervals, mode = "mean", plot = TRUE){
  if (nIntervals < 2){
    stop("'nIntervals' must be a positive integer and greater than 1")
  }
  
  stepSize <- floor(length(f)/nIntervals)
  y <- NULL
  x <- 1
  
  if (mode == "mean" | mode == "MEAN"){
    for (i in 1:nIntervals){
      y[i] <- mean(f[x:(i*stepSize)])
      x <- x + stepSize
    }
  } else if (mode == "sum" | mode == "SUM"){
    for (i in 1:nIntervals){
      y[i] <- sum(f[x:(i*stepSize)])
      x <- x + stepSize
    }
  } else stop("Select either 'mean' or 'sum' for the binning mode")
  
  Time <- 0
  for (i in 2:nIntervals){
    Time[i] <- (Time[i-1]) + acqTime*stepSize
  }
  
  if (plot){
    if (mode == "mean" | mode == "MEAN"){
      plot(y~Time, xlab = "Time (s)", ylab = "Mean counts", type = "l",
           main = paste0("Binned data (averaged), ", nIntervals, " intervals of ", stepSize, " points each"), cex.main = 0.9)
    } else plot(y~Time, xlab = "Time (s)", ylab = "Total counts", type = "l",
                main = paste0("Binned data (added), ", nIntervals, " intervals of ", stepSize, " points each"), cex.main = 0.9)
  }
  
  return(data.frame(Time = Time, Counts = y))
}