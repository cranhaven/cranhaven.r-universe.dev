#' Apply 'Notch' filter
#' @details Mainly used to remove electrical line frequencies
#' at 60, 120, and 180 \code{Hz}.
#' @param s numerical vector if \code{domain=1} (voltage
#' signals), or complex vector if \code{domain=0}
#' @param sample_rate sample rate
#' @param lb filter lower bound of the frequencies to remove
#' @param ub filter upper bound of the frequencies to remove;
#' shares the same length as \code{lb}
#' @param domain \code{1} if the input signal is in the
#' time domain, \code{0} if it is in the frequency domain
#' @returns filtered signal in time domain (real numerical
#' vector)
#' @examples
#'
#'
#' time <- seq(0, 3, 0.005)
#' s <- sin(120 * pi * time) + rnorm(length(time))
#'
#' # Welch periodogram shows a peak at 60Hz
#' pwelch(s, 200, plot = 1, log = "y")
#'
#' # notch filter to remove 60Hz
#' s1 <- notch_filter(s, 200, lb = 59, ub = 61)
#' pwelch(s1, 200, plot = 2, log = "y", col = "red")
#'
#'
#' @export
notch_filter <- function(
  s, sample_rate, lb = c(59, 118, 178),
  ub = c(61, 122, 182), domain = 1){

  stopifnot2(length(lb) == length(ub), msg = "Inconsistent length: `lb` vs `ub`")

  max_freq <- sample_rate / 2
  n <- length(s)
  df <- 2 * max_freq / length(s)
  centre_freq <- (lb + ub) / 2
  filter_width <- (-lb + ub)

  x <- seq(0, max_freq, by = df)
  get_kernel <- function(ii){
    lb <- lb[ii]
    ub <- ub[ii]
    centre_freq <- centre_freq[ii]
    filter_width <- filter_width[ii]
    gauss <- exp(-(centre_freq - x)^2 * 10)
    cnt_gauss <- round(centre_freq / df)
    flat_padd <- 0 # flat padding at the max value of the gaussian
    padd_left <- floor(flat_padd/2)
    padd_right <- ceiling(flat_padd/2)

    gauss_left <- gauss[(padd_left+1):cnt_gauss]
    gauss_right <- gauss[-((padd_left+1):cnt_gauss)]

    our_wind <- 1 - c(gauss_left, rep(0, flat_padd), gauss_right)
    n_r <- length(our_wind)
    if(n %% 2 == 0){
      n_r <- n_r - 1
      our_wind <- our_wind[c(1:n_r, n_r:1)]
    }else{
      our_wind <- our_wind[c(1:n_r, (n_r-1):1)]
    }
    our_wind
  }

  kernel <- get_kernel(1)
  for(ii in seq_along(lb)){
    if(ii > 1){
      kernel <- kernel * get_kernel(ii)
    }
  }

  if(domain == 1){
    s <- fftw_r2c(s) / n
  }

  fftw_c2r(s * kernel)

}
