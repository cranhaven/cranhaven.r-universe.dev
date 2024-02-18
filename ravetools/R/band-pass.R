#' @name band_pass
#' @title Band-pass signals
#' @param x input signals, numeric vector or matrix. \code{x} must be
#' row-major if input is a matrix: each row is a channel, and each column is
#' a time-point.
#' @param sample_rate sampling frequency
#' @param lb lower frequency bound of the band-passing filter, must be positive
#' @param ub upper frequency bound of the band-passing filter, must be greater
#' than the lower bound and smaller than the half of sampling frequency
#' @param domain 1 if \code{x} is in time-domain, or 0 if \code{x} is in
#' frequency domain
#' @param method filter type, choices are \code{'fir'} and \code{'butter'}
#' @param direction filter direction, choices are \code{'forward'},
#' \code{'backward'}, and \code{'both'} directions
#' @param window window type, can be a character, a function, or a vector.
#' For character, \code{window} is a function name in the
#' \code{signal} package, for example, \code{'hanning'}; for a function,
#' \code{window} takes one integer argument and returns a numeric vector
#' with length of that input; for vectors, \code{window} is a numeric vector
#' o length \code{order+1}.
#' @param order the order of the filter, must be positive integer and be
#' less than one-third of the sample rate
#' @param ... ignored
#' @returns Filtered signals, vector if \code{x} is a vector, or matrix of
#' the same dimension as \code{x}
#' @examples
#'
#'
#' t <- seq(0, 1, by = 0.0005)
#' x <- sin(t * 0.4 * pi) + sin(t * 4 * pi) + 2 * sin(t * 120 * pi)
#'
#' par(mfrow = c(2, 2), mar = c(3.1, 2.1, 3.1, 0.1))
#' # ---- Using band_pass1 ------------------------------------------------
#'
#' y1 <- band_pass1(x, 2000, 0.1, 1)
#' y2 <- band_pass1(x, 2000, 1, 5)
#' y3 <- band_pass1(x, 2000, 10, 80)
#'
#' plot(t, x, type = 'l', xlab = "Time", ylab = "",
#'      main = "Mixture of 0.2, 2, and 60Hz")
#' lines(t, y1, col = 'red')
#' lines(t, y2, col = 'blue')
#' lines(t, y3, col = 'green')
#' legend(
#'   "topleft", c("Input", "Pass: 0.1-1Hz", "Pass 1-5Hz", "Pass 10-80Hz"),
#'   col = c(par("fg"), "red", "blue", "green"), lty = 1,
#'   cex = 0.6
#' )
#'
#' # plot pwelch
#' pwelch(x, fs = 2000, window = 4000, noverlap = 2000, plot = 1)
#' pwelch(y1, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "red")
#' pwelch(y2, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "blue")
#' pwelch(y3, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "green")
#'
#'
#' # ---- Using band_pass2 with FIR filters --------------------------------
#'
#' order <- floor(2000 / 3)
#' z1 <- band_pass2(x, 2000, 0.1, 1, method = "fir", order = order)
#' z2 <- band_pass2(x, 2000, 1, 5, method = "fir", order = order)
#' z3 <- band_pass2(x, 2000, 10, 80, method = "fir", order = order)
#'
#' plot(t, x, type = 'l', xlab = "Time", ylab = "",
#'      main = "Mixture of 0.2, 2, and 60Hz")
#' lines(t, z1, col = 'red')
#' lines(t, z2, col = 'blue')
#' lines(t, z3, col = 'green')
#' legend(
#'   "topleft", c("Input", "Pass: 0.1-1Hz", "Pass 1-5Hz", "Pass 10-80Hz"),
#'   col = c(par("fg"), "red", "blue", "green"), lty = 1,
#'   cex = 0.6
#' )
#'
#' # plot pwelch
#' pwelch(x, fs = 2000, window = 4000, noverlap = 2000, plot = 1)
#' pwelch(z1, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "red")
#' pwelch(z2, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "blue")
#' pwelch(z3, fs = 2000, window = 4000, noverlap = 2000,
#'        plot = 2, col = "green")
#'
#'
#'
#' @export
band_pass1 <- function(x, sample_rate, lb, ub, domain = 1, ...) {

  max_freq <- sample_rate / 2

  if(lb >= ub) {
    stop("`band_pass`: Band-passing filtering frequency must have lower bound < upper bound")
  }
  if( lb < 0 ) {
    stop("`band_pass`: Band-passing frequency lower bound must be positive")
  }
  if( max_freq < ub ) {
    stop("`band_pass`: Band-passing frequency higher bound must not exceed half of sampling frequency")
  }

  # check if x is a matrix
  if(!is.matrix(x)) {
    x_is_vec <- TRUE
    x <- matrix(x, nrow = 1)
  } else {
    x_is_vec <- FALSE
  }
  n_tp <- ncol(x)


  df <- sample_rate / n_tp

  center_freq <- (lb + ub) / 2
  filter_width <- ub - lb

  y <- seq(0, max_freq, by = df)
  gauss <- exp( - (y - center_freq)^2 )
  cnt_gauss <- round( center_freq / df )
  flat_padd <- round( filter_width / df )

  padd_left <- floor( flat_padd / 2 )
  padd_right <- ceiling( flat_padd / 2 )

  # if( padd_left >= cnt_gauss ) {
  #   padd_left <- cnt_gauss - 1
  #   padd_right <- flat_padd - cnt_gauss + 1
  # }
  # if( length(gauss) - padd_right < cnt_gauss + 1) {
  #   padd_right <- length(gauss) - cnt_gauss - 1
  #   padd_left <- cnt_gauss + flat_padd + 1 - length(gauss)
  # }

  our_wind <- c(
    gauss[ seq(padd_left+1, cnt_gauss) ],
    rep( 1.0, flat_padd ),
    gauss[ seq(cnt_gauss+1, length(gauss) - padd_right) ]
  )

  if( n_tp %% 2 == 0 ) {
    our_wind <- our_wind[-length(our_wind)]
    our_wind <- c(our_wind, rev(our_wind))
  } else {
    our_wind <- c(our_wind[-length(our_wind)], rev(our_wind))
  }

  re <- apply(x, 1, function(slice) {
    if(domain > 0) {
      slice <- fft(slice)
    }
    slice <- slice * our_wind
    ifft(slice)
  })
  if(x_is_vec) {
    re <- drop(re)
  } else {
    re <- t(re)
  }
  re
}

#' @rdname band_pass
#' @export
band_pass2 <- function(x, sample_rate, lb, ub, order,
                       method = c("fir", "butter"),
                       direction = c("both", "forward", "backward"),
                       window = "hamming",
                       ...) {

  method <- match.arg(method)
  direction <- match.arg(direction)

  if( missing(order) ) {
    if(method == "fir") {
      order <- 25
    } else {
      order <- 4
    }
  } else {
    if( length(order) != 1 || !is.numeric(order) || is.na(order) || order != as.integer(order) || order < 0 ) {
      stop("`band_pass2`: filter order must be an positive integer")
    }
  }

  if(is.character(window)) {
    signal <- asNamespace("signal")
    window <- signal[[window]]
  }


  fn <- sample_rate / 2
  if(lb >= ub) {
    stop("`band_pass`: Band-passing filtering frequency must have lower bound < upper bound")
  }
  if( lb < 0 ) {
    stop("`band_pass`: Band-passing frequency lower bound must be positive")
  }
  if( fn < ub ) {
    stop("`band_pass`: Band-passing frequency higher bound must not exceed half of sampling frequency")
  }

  w <- c(lb, ub) / fn
  if(method == "fir") {
    b <- fir1(order, w = w, type = "pass", window = window)
    a <- 1
  } else {
    if(is.function(window)) {
      window <- window(order + 1)
    }
    bf <- signal::butter(order, w, type = "pass", window = window)
    b <- bf$b
    a <- bf$a
  }

  # check if x is a matrix
  if(!is.matrix(x)) {
    x_is_vec <- TRUE
    x <- matrix(x, nrow = 1)
  } else {
    x_is_vec <- FALSE
  }

  re <- apply(x, 1, function(slice) {
    switch(
      direction,
      "forward" = {
        re <- filter_signal(b = b, a = a, x = slice)
        re[[1]]
      },
      "backward" = {
        re <- filter_signal(b = b, a = a, x = rev(slice))
        rev(re[[1]])
      },
      {
        filtfilt(b = b, a = a, x = slice)
      }
    )
  })

  if( x_is_vec ) {
    re <- drop(re)
  } else {
    re <- t(re)
  }

  re
}
