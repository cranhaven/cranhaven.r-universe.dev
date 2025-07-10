#' Plot method for psd class
#' @description This function plots the log periodogram, log posterior median PSD, and log 90\% credible region PSD.  The x-axis uses angular frequency and the y-axis is plotted on the log scale.  The PSD at the zero frequency is removed from the plot.  If the time series is even length, the PSD at the last frequency is also removed from the plot.
#' @export
#' @param x an object of class psd
#' @param legend.loc location of legend out of "topright" (default), "topleft", "bottomright", "bottomleft".  If set to NA then no legend will be produced
#' @param ylog logical value (default is TRUE) to determine if PSD (y-axis) should be on natural log scale
#' @param ... other graphical parameters from the plot.default function
#' @return plot of the estimate of the (log) PSD
#' @seealso \link{gibbs_bspline}
#' @examples 
#' \dontrun{
#' 
#' set.seed(12345)
#' 
#' # Simulate AR(4) data
#' n = 2 ^ 7
#' ar.ex = c(0.9, -0.9, 0.9, -0.9)
#' data = arima.sim(n, model = list(ar = ar.ex))
#' data = data - mean(data)
#' 
#' # Run MCMC with linear B-spline prior (may take some time)
#' mcmc = gibbs_bspline(data, 10000, 5000, degree = 1)
#' 
#' # Plot result
#' plot(mcmc)
#' 
#' # Plot result on original scale with title
#' plot(mcmc, ylog = FALSE, main = "Estimate of PSD using the linear B-spline prior")
#' }
plot.psd = function(x, legend.loc = "topright", ylog = TRUE, ...) {  # Plot method for "psd" class
  N = length(x$pdgrm)
  freq = seq(0, pi, length = N)
  # Frequencies to remove from estimate
  if (x$n %% 2) {  # Odd length time series
    bFreq = 1
  }
  else {  # Even length time series
    bFreq = c(1, N)
  }
  
  if (!is.logical(ylog)) stop("ylog must be TRUE or FALSE")
  
  if (ylog == TRUE) {
    graphics::plot.default(freq[-bFreq], log(x$pdgrm[-bFreq]), type = "l", col = "grey",
                           xlab = "Frequency", ylab = "log PSD", ...)
    graphics::lines(freq[-bFreq], log(x$psd.median[-bFreq]), lwd = 2)
    graphics::lines(freq[-bFreq], log(x$psd.p05[-bFreq]), lwd = 2, lty = 2, col = 4)
    graphics::lines(freq[-bFreq], log(x$psd.p95[-bFreq]), lwd = 2, lty = 2, col = 4)
    if (!is.na(legend.loc)) {
      graphics::legend(legend.loc, legend = c("periodogram", "posterior median", "90% credible region"), 
                       col = c("grey", "black", "blue"), lwd = c(1, 2, 2), lty = c(1, 1, 2))  
    }
  }
  if (ylog == FALSE) {
    graphics::plot.default(freq[-bFreq], x$pdgrm[-bFreq], type = "l", col = "grey",
                           xlab = "Frequency", ylab = "PSD", ...)
    graphics::lines(freq[-bFreq], x$psd.median[-bFreq], lwd = 2)
    graphics::lines(freq[-bFreq], x$psd.p05[-bFreq], lwd = 2, lty = 2, col = 4)
    graphics::lines(freq[-bFreq], x$psd.p95[-bFreq], lwd = 2, lty = 2, col = 4)
    if (!is.na(legend.loc)) {
      graphics::legend(legend.loc, legend = c("periodogram", "posterior median", "90% credible region"), 
                       col = c("grey", "black", "blue"), lwd = c(1, 2, 2), lty = c(1, 1, 2))  
    }
  }
}