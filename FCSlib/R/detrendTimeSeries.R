#' @title Algorithms for data detrending
#' 
#' @description Allows to perform Exponential, Polynomial or Boxcar Filter detrending over a time series vector
#' @aliases detrendTimeSeries
#' @usage detrendTimeSeries(f, acqTime, nIntervals, algorithm, degree, w, pois = FALSE,
#'  max = FALSE, plot = TRUE)
#' @param f A vector
#' @param acqTime Point acquisition rate (in seconds)
#' @param nIntervals Number of intervals into which the vector will be grouped prior to the detrending process
#' @param algorithm A character string. Choose between Exponential ("exp"), Polynomial ("poly") or Boxcar Filter ("boxcar") detrending
#' @param degree The degree of the polynomial function
#' @param w Moving average time window size
#' @param pois Logical, set to TRUE for detrending performance by adding random, uncorrelated numbers sampled from a Poisson distribution (see details)
#' @param max  Logical, set to TRUE for detrending performance based on the highest value of the original data, rather than the first one (see details)
#' @param plot Logical, set to TRUE (default) to plot de result
#' @details First, the binTimeSeries() function is used to obtain a binned version of 'f' of 'nIntervals' points.
#'  
#' For exponential detrending, a model of the form (A0*e^(k*t) is adjusted to the binned vector.
#' 
#' A polynomial function of user-specified degree is rather used for polynomial detrending.
#' 
#' For the case of boxcar filtering, the moving average vector is calculated from the original series.
#' An amount of zeroes equal to (w-1) is added at the tail of 'f' to compensate for the moving average
#' effect when position (length(f) - w + 1) is reached.
#' 
#' In either case, the residuals are then obtained and added a constant value for trend correction.
#' When 'max' is set to TRUE, said value will be the highest in the binned vector of 'f'.
#' 
#' When 'pois' is set to FALSE (default), the trend correction value is directly added to the obtained residuals,
#' in a quantity that will make the average counts remain constant throughout the whole time series.
#' On the other hand, when 'pois = TRUE', the trend correction value is instead used as the 'lambda'
#' parameter for a Poisson distribution from which uncorrelated counts will be randomly sampled and
#' added to the whole series for trend correction. This procedure asures that only integer counts will
#' be obtained after detrending, at the cost of adding some noise and making the detrending process a lengthier task. 
#' 
#' @export
#' @importFrom graphics abline lines points
#' @importFrom stats lm rpois
#' @return A vector
#' @author Alejandro Linares, Ad?n Guerrero, Haydee Hern?ndez
#' 
#' @seealso \code{\link{binTimeSeries}}
#' 
#' @examples
#' \donttest{
#' ### Please navigate to
#' ### (https://github.com/FCSlib/FCSlib/tree/master/Sample%20Data)
#' ### to find this sample data
#' 
#' x <- read.table("PB030.dat", header = F)
#' 
#' # Exponential detrending
#' x.d <- detrendTimeSeries(data$V2, 4e-6, 100, algorithm = "exp", pois = F, max = F)
#' 
#' # Polynomial detrending
#' x.d <- detrenddetrendTimeSeries(data$V2, 4e-6, 100, algorithm = "poly", degree = 5, pois = F, max = F)
#' 
#' # Boxcar Filter detrending
#' x.d <- <- detrenddetrendTimeSeries(data$V2, 4e-6, 100, algorithm = "boxcar", w = 100000, pois = F, max = F)
#' 
#' # Automatic plot disabled
#' exp <- detrendTimeSeries(data$V2, acqTime = 4e-6, nIntervals = 100, algorithm = "exp", plot = FALSE)
#' 
#' # Graphing the result (binned)
#' exp.b <- binTimeSeries(exp, acqTime = 4e-6, nIntervals = 100, plot = FALSE)
#' plot(exp.b$Counts~exp.b$Counts, ylab = "Custom 'Y' axis label", xlab = "Custom 'X' axis label",
#' main = "Custom graph title", lwd = 2, type = "l", col = "green4", ylim = c(0,0.3))
#' legend("topright", legend = "detrended data", col = "green4", lwd = 2)
#' 
#' # Graphing the result (raw, first 10000 points)
#' plot(exp[1:10000]~data$V1[1:10000], ylab = "Custom 'Y' axis label",
#' xlab = "Custom 'X' axis label", main = "Custom graph title", type = "l", col = "green4")
#' legend("topright", legend = "detrended data", col = "green4", lwd = 2)
#' 

detrendTimeSeries <- function(f, acqTime, nIntervals, algorithm, degree, w, pois = FALSE, max = FALSE, plot = TRUE){
  a <- binTimeSeries(f, acqTime, nIntervals, plot = F)
  
  if (algorithm == "exp" | algorithm == "EXP"){
    em <- lm(log(a$Counts)~a$Time)
    A0 <- exp(em$coefficients[1])
    k <- em$coefficients[2]
    b <- model <- A0*exp(k*a$Time)
    t <- (1:length(f))*acqTime
    full.model <- A0*exp(k*t)
    if (max){h <- exp(max(em$fitted.values))} else h <- exp(em$fitted.values[1])
    if (pois){
      full.residuals <- NULL
      if (max){
        for (i in 1:length(full.model)){
          full.residuals[i] <- f[i] + rpois(1, abs(exp(max(em$fitted.values)) - full.model[i]))
        }
      } else
        for (i in 1:length(full.model)){
          full.residuals[i] <- f[i] + rpois(1, abs(exp(em$fitted.values[1]) - full.model[i]))
        }
    } else if (!pois){
      if (max){
        full.residuals <- f - full.model + exp(max(em$fitted.values))
      } else full.residuals <- f - full.model + exp(em$fitted.values[1])
    }
    
  } else if (algorithm == "poly" | algorithm == "POLY"){
    pm <- lm(a$Counts~poly(a$Time, degree = degree))
    b <- as.vector(pm$fitted.values)
    if (max){h <- max(b)} else h <- b[1]
    t <- (1:length(f))*acqTime
    full.model <- lm(f~poly(t, degree = degree))
    if (pois){
      full.residuals <- NULL
      if (max){
        for (i in 1:length(f)){
          full.residuals[i] <- full.model$residuals[i] + rpois(1, max(b))
        }
      } else
        for (i in 1:length(f)){
          full.residuals[i] <- full.model$residuals[i] + rpois(1, b[1])
        }
    } else if (!pois){
      if (max){
        full.residuals <- full.model$residuals + max(b)
      } else full.residuals <- full.model$residuals + b[1]
    }
    
  } else if (algorithm == "boxcar" | algorithm == "BOXCAR"){
    if (w > (length(f)/2) | w <= 1){
      stop("'w' must be greater than 1 and smaller than half the length of 'f'")
    }
    w <- floor(w)
    x <- rep(0, length(f))
    f_zp <- c(f, rep(0, w - 1))
    for (i in 1:(length(f)-w+1)){
      x[i] <- mean(f_zp[i:i+w-1])
    }
    b <- (binTimeSeries(x, acqTime, nIntervals, plot = F))$Counts
    if (max){h <- max(b)} else h <- b[1]
    if (pois){
      if (max){
        full.residuals <- f - x + rpois(length(f), max(b))
      } else full.residuals <- f - x + rpois(length(f), b[1])
    } else if (!pois){
      if (max){
        full.residuals <- f - x + max(b)
      } else full.residuals <- f - x + b[1]
    }
  } else stop("Select a valid detrending algorithm ('exp', 'poly' or 'boxcar')")
  
  c <- binTimeSeries(full.residuals, acqTime, nIntervals, plot = F)
  
  if (plot){
    plot(a$Counts~a$Time, type = "l", ylim = c(min(a$Counts),max(c$Counts)),
         xlab = "Time (s)", ylab = "Mean counts", main = "Detrended single-point data")
    lines(b~a$Time, lwd = 2, col = "red")
    lines(c$Counts~c$Time, col = "blue")
    abline(h = h, lwd = 2)
  }
  
  return(full.residuals)
}