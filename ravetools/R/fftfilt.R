# rewrite part of signal package

fft <- function(x, inverse = FALSE){
  if(inverse){
    fftw_c2r(x)
  } else {
    fftw_r2c(x)
  }
}
ifft <- function(x){
  fftw_c2r(x) / length(x)
}

fftfilt <- function (b, x, n = NULL) {
  N <- n
  l_x <- length(x)
  l_b <- length(b)
  if (is.null(n)) {
    N <- 2^(ceiling(log(l_x + l_b - 1)/log(2)))
    B <- fft(postpad(b, N))
    y <- ifft(fft(postpad(x, N)) * B)
  } else {
    if (length(n) > 1) {
      stop("fftfilt: n has to be a scalar")
    }
    N <- 2^(ceiling(log(max(N, l_b))/log(2)))
    L <- N - l_b + 1
    B <- fft(postpad(b, N))
    R <- ceiling(l_x/L)
    y <- numeric(l_x)
    for (r in 1:R) {
      lo <- (r - 1) * L + 1
      hi <- min(r * L, l_x)
      tmp <- numeric(0)
      tmp[1:(hi - lo + 1)] <- x[lo:hi]
      tmp <- ifft(fft(postpad(tmp, N)) * B)
      hi <- min(lo + N - 1, l_x)
      y[lo:hi] <- y[lo:hi] + tmp[1:(hi - lo + 1)]
    }
  }
  y <- y[1:l_x]
  if (is.numeric(b) && is.numeric(x))
    y <- Re(y)
  if (!any(as.logical(b - round(b)))) {
    idx <- !any(as.logical(x - round(x)))
    y[idx] <- round(y[idx])
  }
  y
}

hamming <- function (n) {
  if (!(n == round(n) && n > 0))
    stop("hamming: n has to be an integer > 0")
  if (n == 1)
    c <- 1
  else {
    n <- n - 1
    c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
  }
  c
}

fir2 <- function (n, f, m, grid_n = 512,
                  ramp_n = grid_n/20,
                  window = hamming(n + 1))
{
  t <- length(f)
  if (t < 2 || f[1] != 0 || f[t] != 1 || any(diff(f) < 0))
    stop("frequency must be nondecreasing starting from 0 and ending at 1")
  if (t != length(m))
    stop("frequency and magnitude vectors must be the same length")
  if (length(grid_n) > 1 || length(ramp_n) > 1)
    stop("grid_n and ramp_n must be integers")
  if (length(window) != n + 1)
    stop("window must be of length n+1")
  if (2 * grid_n < n + 1)
    grid_n <- 2^ceiling(log2(abs(n + 1)))
  if (ramp_n > 0) {
    basef <- f
    basem <- m
    idx <- which(diff(f) == 0)
    f[idx] <- f[idx] - ramp_n/grid_n/2
    f[idx + 1] <- f[idx + 1] + ramp_n/grid_n/2
    f <- c(f, basef[idx])
    f[f < 0] <- 0
    f[f > 1] <- 1
    f <- sort(unique(c(f, basef[idx])))
    m <- approx(basef, basem, f, ties = "ordered")$y
  }
  grid <- approx(f, m, seq(0, 1, length = grid_n + 1), ties = "ordered")$y
  if ((n%%2) == 0) {
    b <- ifft(c(grid, grid[seq(grid_n, 2, by = -1)]))
    mid <- (n + 1)/2
    b <- Re(c(b[(2 * grid_n - floor(mid) + 1):(2 * grid_n)],
             b[1:ceiling(mid)]))
  } else {
    b <- ifft(c(grid, rep(0, grid_n * 2), grid[seq(grid_n, 2, by = -1)]))
    b <- 2 * Re(c(b[seq(length(b) - n + 1, length(b), by = 2)],
                 b[seq(2, n + 2, by = 2)]))
  }
  b <- b * window
  b
}

polyval <- function (coef, z)
{
  lz <- length(z)
  if (!lz)
    return(numeric(0))
  n <- length(coef)
  if (!n) {
    z[] <- 0
    return(z)
  }
  if (!(mode(coef) == "numeric") && !(mode(coef) == "complex"))
    stop("Argument 'coef' must be a real or complex vector.")
  d_z <- dim(z)
  dim(z) <- lz
  y <- outer(z, (n - 1):0, "^") %*% coef
  dim(y) <- d_z
  return(y)
}

# fir1 <- function (
#   n, w, type = c("low", "high", "stop", "pass", "DC-0", "DC-1"),
#   window = hamming(n + 1), scale = TRUE)
# {
#   type <- match.arg(type)
#   if (!is.logical(scale)) {
#     scale <- match.arg(scale, c("scale", "noscale"))
#     scale <- scale == "scale"
#   }
#   if (is.function(window))
#     window <- window(n + 1)
#   else if (is.character(window))
#     window <- do.call(window, list(n + 1))
#   ftype <- tolower(type) %in% c("low", "stop", "dc-1")
#   bands <- length(w) + 1
#   f <- numeric(2 * bands)
#   f[2 * bands] <- 1
#   f[seq(2, 2 * bands - 1, by = 2)] <- w
#   f[seq(3, 2 * bands - 1, by = 2)] <- w
#   m <- numeric(2 * bands)
#   m[seq(1, 2 * bands, by = 2)] <- (1:bands - (1 - ftype))%%2
#   m[seq(2, 2 * bands, by = 2)] <- m[seq(1, 2 * bands, by = 2)]
#   if (n%%2 == 1 && m[2 * bands] == 1) {
#     stop("n must be even for highpass and bandstop filters.")
#   }
#   b <- fir2(n, f, m, 512, 2, window)
#   if (scale) {
#     if (m[1] == 1) {
#       w_o <- (f[2] - f[1])/2
#     } else {
#       w_o <- f[3] + (f[4] - f[3])/2
#     }
#     renorm <- 1/abs(polyval(b, exp(-(0+1i) * pi * w_o)))
#     b <- renorm * b
#     # b <- b / sum(b)
#   }
#   b
# }
#


#' Decimate with 'FIR' or 'IIR' filter
#' @param x signal to be decimated
#' @param q integer factor to down-sample by
#' @param n filter order used in the down-sampling; default is \code{30}
#' if \code{ftype='fir'}, or \code{8} if \code{ftype='iir'}
#' @param ftype filter type, choices are \code{'fir'} (default) and
#' \code{'iir'}
#' @returns Decimated signal
#' @details This function is migrated from \code{signal} package,
#' but with bugs fixed on 'FIR' filters. The result agrees with 'Matlab'
#' \code{decimate} function with 'FIR' filters. Under 'IIR'
#' filters, the function is identical with \code{signal::decimate},
#' and is slightly different with 'Matlab' version.
#'
#' @examples
#'
#' x <- 1:100
#' y <- decimate(x, 2, ftype = "fir")
#' y
#'
#' # compare with signal package
#' z <- signal::decimate(x, 2, ftype = "fir")
#'
#' # Compare decimated results
#' plot(x, type = 'l')
#' points(seq(1,100, 2), y, col = "green")
#' points(seq(1,100, 2), z, col = "red")
#'
#'
#' @export
decimate <- function (
  x, q, n = if (ftype == "iir") 8 else 30, ftype = "fir") {
  if (q != round(q))
    stop("decimate only works with integer q.")
  l_x <- length(x)


  if(ftype == "fir"){
    npad <- ceiling(n / 2)
    lpad <- 2*x[1] - x[(npad+1):2]
    rpad <- 2*x[l_x] - x[l_x - (1:npad)]
    inp <- c(lpad, x, rpad)

    b <- fir1(n, 1/q)
    y <- fftfilt(b, inp)
    y <- y[ceiling(npad + n/2) + (1:l_x)]
    y <- y[seq(1, length(x), by = q)]
  } else {
    y <- signal::decimate(x, q, n, "iir")
  }

  y

}
