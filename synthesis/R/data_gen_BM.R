#' Generate a time series of Brownian motion.
#'
#' This function generates a time series of one dimension Brownian motion.
#'
#' @param x0 the start value of x, with the default value 0
#' @param w0 the start value of w, with the default value 0
#' @param time the temporal interval at which the system will be generated. Default seq(0,by=0.01,len=101).
#' @param do.plot a logical value. If TRUE (default value), a plot of the generated system is shown.
#'
#' @export
#'
#' @references Yanping Chen, http://cos.name/wp-content/uploads/2008/12/stochastic-differential-equation-with-r.pdf
#'
#' @examples
#' set.seed(123)
#' x <- data.gen.bm()
data.gen.bm <- function(x0=0, w0=0, time=seq(0, by=0.01, length.out = 101), do.plot=TRUE)
{
  delta <- diff(time)[1]
  n <- length(time)
  W <- numeric(n)
  t0 <- time[1]

  W[1] <- w0
  for(i in 2:n) W[i] <- W[i-1] + rnorm(1) * sqrt(delta)
  x <- ts(W, start=t0, deltat = delta)

  # plotting
  if (do.plot) {
    title = paste("Brownian motion\n", "x0 = ", x0, "dt = ", delta)
    plot(x, xlab = "t", ylab = "x", main = title, type = "l")
  }

  return(x)
}


#' Generate a time series of geometric Brownian motion.
#'
#' This function generates a a time series of one dimension geometric Brownian motion.
#'
#' @param x0 the start value of x, with the default value 10
#' @param w0 the start value of w, with the default value 0
#' @param mu the interest/drifting rate, with the default value 1.
#' @param sigma the diffusion coefficient, with the default value 0.5.
#' @param time the temporal interval at which the system will be generated. Default seq(0,by=0.01,len=101).
#' @param do.plot a logical value. If TRUE (default value), a plot of the generated system is shown.
#'
#' @export
#'
#' @references Yanping Chen, http://cos.name/wp-content/uploads/2008/12/stochastic-differential-equation-with-r.pdf
#'
#' @examples
#' set.seed(123)
#' x <- data.gen.gbm()
data.gen.gbm <- function(x0=10, w0=0, mu=1, sigma=0.5, time=seq(0, by=0.01, length.out = 101), do.plot=TRUE)
{
  delta <- diff(time)[1]
  n <- length(time)
  W <- numeric(n)
  t0 <- time[1]

  W[1] <- w0
  for(i in 2:n) W[i] <- W[i-1] + rnorm(1) * sqrt(delta)
  S <- x0 * exp((mu-sigma^2/2)*(time-t0) + sigma*(W-W[1]))
  x <- ts(S, start=t0, deltat = delta)

  # plotting
  if (do.plot) {

    title = paste("Geometric Brownian motion\n", "x0 = ", x0, " dt = ", delta)
    plot(x, xlab = "t", ylab = "x", main = title, type = "l")
  }

  return(x)
}

#' Generate a time series of fractional Brownian motion.
#'
#' This function generates a a time series of one dimension fractional Brownian motion.
#'
#' @param hurst the hurst index, with the default value 0.95, ranging from [0,1].
#' @param time the temporal interval at which the system will be generated. Default seq(0,by=0.01,len=1000).
#' @param do.plot a logical value. If TRUE (default value), a plot of the generated system is shown.
#'
#' @importFrom stats fft
#' @export
#'
#' @references
#' Zdravko Botev (2020). Fractional Brownian motion generator (https://www.mathworks.com/matlabcentral/fileexchange/38935-fractional-brownian-motion-generator), MATLAB Central File Exchange. Retrieved August 17, 2020.
#'
#' Kroese, D. P., & Botev, Z. I. (2015). Spatial Process Simulation. In Stochastic Geometry, Spatial Statistics and Random Fields(pp. 369-404) Springer International Publishing, DOI: 10.1007/978-3-319-10064-7_12
#'
#' @examples
#' set.seed(123)
#' x <- data.gen.fbm()
data.gen.fbm <- function(hurst=0.95, time=seq(0, by=0.01, length.out=1000), do.plot=TRUE)
{
  # Catch errors in inputs
  if(hurst<0|hurst>1) stop('Error: Hurst parameter must be between 0 and 1')

  delta <- diff(time)[1]
  n <- length(time)
  r <- numeric(n+1)

  r[1] <- 1
  for(k in 1:n) r[k+1] <- 0.5 * ((k+1)^(2*hurst) - 2*k^(2*hurst) + (k-1)^(2*hurst))
  r <- c(r, r[seq(length(r)-1, 2)])
  lambda <- Re((fft(r)) / (2*n)) # eigenvalues
  W <- fft(sqrt(lambda) * (rnorm(2*n) + rnorm(2*n)*1i))
  W <- n^(-hurst) * cumsum(Re(W[1:n])) # rescale
  W=(time[n]^hurst)*W # scale for final time T

  x <- ts(W, start=time[1], deltat = delta)

  # plotting
  if (do.plot) {
    title = paste("Fractional Brownian motion\n", "hurst = ", hurst, "dt = ", delta)
    plot(x, xlab = "t", ylab = "x", main = title, type = "l")
  }

  return(x)
}
