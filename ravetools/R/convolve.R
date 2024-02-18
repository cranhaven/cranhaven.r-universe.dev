#' @name convolve
#' @title Convolution of \code{1D}, \code{2D}, \code{3D} data via \code{FFT}
#' @description Use the 'Fast-Fourier' transform to compute the convolutions of
#' two data with zero padding.
#' @param x one-dimensional signal vector, two-dimensional image, or
#' three-dimensional volume; numeric or complex
#' @param filter kernel with the same number of dimensions as \code{x}
#' @returns Convolution results with the same length and dimensions as \code{x}.
#' If \code{x} is complex, results will be complex, otherwise results will
#' be real numbers.
#' @details This implementation uses 'Fast-Fourier' transform to perform
#' \code{1D}, \code{2D}, or \code{3D} convolution. Compared to implementations
#' using original mathematical definition of convolution, this approach is
#' much faster, especially for image and volume convolutions.
#'
#' The input \code{x} is zero-padded beyond edges. This is most common in image
#' or volume convolution, but less optimal for periodic one-dimensional signals.
#' Please use other implementations if non-zero padding is needed.
#'
#' The convolution results might be different to the ground truth by a precision
#' error, usually at \code{1e-13} level, depending on the \code{'FFTW3'}
#' library precision and implementation.
#'
#' @examples
#'
#'
#' # ---- 1D convolution ------------------------------------
#' x <- cumsum(rnorm(100))
#' filter <- dnorm(-2:2)
#' # normalize
#' filter <- filter / sum(filter)
#' smoothed <- convolve_signal(x, filter)
#'
#' plot(x, pch = 20)
#' lines(smoothed, col = 'red')
#'
#' # ---- 2D convolution ------------------------------------
#' x <- array(0, c(100, 100))
#' x[
#'   floor(runif(10, min = 1, max = 100)),
#'   floor(runif(10, min = 1, max = 100))
#' ] <- 1
#'
#' # smooth
#' kernel <- outer(dnorm(-2:2), dnorm(-2:2), FUN = "*")
#' kernel <- kernel / sum(kernel)
#'
#' y <- convolve_image(x, kernel)
#'
#' par(mfrow = c(1,2))
#' image(x, asp = 1, axes = FALSE, main = "Origin")
#' image(y, asp = 1, axes = FALSE, main = "Smoothed")
#'
#'
NULL

#' @rdname convolve
#' @export
convolve_signal <- function(x, filter) {

  len_x <- length(x)
  len_y <- length(filter)

  padded_len <- len_x + len_y + 1L

  junk_len <- ceiling(len_y / 2) - 1

  x_ <- c(as.double(x), rep(0.0, len_y + 1L))
  y_ <- c(as.double(filter), rep(0.0, len_x + 1L))

  a <- fftw_c2c(x_)
  b <- fftw_c2c(y_)

  a <- fftw_c2c(a * b, inverse = TRUE, ret = a) / padded_len
  b <- a[junk_len + seq_len(len_x)]

  if(!is.complex(x)) {
    b <- Re(b)
  }
  b
}

#' @rdname convolve
#' @export
convolve_image <- function(x, filter) {
  # make sure x and filter are matrix
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }

  if(!is.matrix(filter)) {
    filter <- as.matrix(filter)
  }

  dim_x <- dim(x)
  dim_y <- dim(filter)

  padded_dim <- dim_x + dim_y + 1L

  junk_dim <- ceiling(dim_y / 2) - 1

  x_ <- array(0.0, padded_dim)
  x_[ seq_len(dim_x[[1]]), seq_len(dim_x[[2]]) ] <- x
  y_ <- array(0.0, padded_dim)
  y_[ seq_len(dim_y[[1]]), seq_len(dim_y[[2]]) ] <- filter

  a <- fftw_c2c_2d(x_)
  b <- fftw_c2c_2d(y_)

  a <- fftw_c2c_2d(a * b, inverse = TRUE, ret = a) / prod(padded_dim)
  b <- a[junk_dim[[1]] + seq_len(dim_x[[1]]),
         junk_dim[[2]] + seq_len(dim_x[[2]]),
         drop = FALSE]

  if(!is.complex(x)) {
    b <- Re(b)
  }
  b
}

#' @rdname convolve
#' @export
convolve_volume <- function(x, filter) {
  # make sure x and filter are matrix
  if(!is.array(x)) {
    x <- is.array(x)
  }

  if(!is.array(filter)) {
    filter <- is.array(filter)
  }

  dim_x <- dim(x)
  dim_y <- dim(filter)
  if(length(dim_x) < 3L) {
    dim_x <- c(dim_x, rep(1, 3 - length(dim_x)))
  } else if(length(dim_x) > 3L) {
    if(prod(dim_x) != prod(dim_x[1:3])) {
      stop("`convolve_volume`: x must be an array with 3 dimensions")
    }
    dim_x <- dim_x[1:3]
    dim(x) <- dim_x
  }
  if(length(dim_y) < 3L) {
    dim_y <- c(dim_y, rep(1, 3 - length(dim_y)))
  } else if(length(dim_y) > 3L) {
    if(prod(dim_y) != prod(dim_y[1:3])) {
      stop("`convolve_volume`: filter must be an array with 3 dimensions")
    }
    dim_y <- dim_y[1:3]
    dim(filter) <- dim_y
  }

  padded_dim <- dim_x + dim_y + 1L
  junk_dim <- ceiling(dim_y / 2) - 1L

  x_ <- array(0.0, padded_dim)
  x_[
    seq_len(dim_x[[1]]),
    seq_len(dim_x[[2]]),
    seq_len(dim_x[[3]])
  ] <- x
  y_ <- array(0.0, padded_dim)
  y_[
    seq_len(dim_y[[1]]),
    seq_len(dim_y[[2]]),
    seq_len(dim_y[[3]])
  ] <- filter

  a <- fftw_c2c_3d(x_)
  b <- fftw_c2c_3d(y_)

  a <- fftw_c2c_3d(a * b, inverse = TRUE, ret = a) / prod(padded_dim)
  b <- a[
    junk_dim[[1]] + seq_len(dim_x[[1]]),
    junk_dim[[2]] + seq_len(dim_x[[2]]),
    junk_dim[[3]] + seq_len(dim_x[[3]]),
    drop = FALSE
  ]

  if(!is.complex(x)) {
    b <- Re(b)
  }
  b
}


#' Grow volume mask
#' @param volume volume mask array, must be 3-dimensional array
#' @param x,y,z size of grow along each direction
#' @param threshold threshold after convolution
#' @examples
#'
#'
#' par(mfrow = c(2,3), mar = c(0.1,0.1,3.1,0.1))
#'
#' mask <- array(0, c(21,21,21))
#' mask[11,11,11] <- 1
#' image(mask[11,,], asp = 1,
#'       main = "Original mask", axes = FALSE)
#' image(grow_volume(mask, 2)[11,,], asp = 1,
#'       main = "Dilated (size=2) mask", axes = FALSE)
#' image(grow_volume(mask, 5)[11,,], asp = 1,
#'       main = "Dilated (size=5) mask", axes = FALSE)
#'
#' mask[11, sample(11,2), sample(11,2)] <- 1
#' image(mask[11,,], asp = 1,
#'       main = "Original mask", axes = FALSE)
#' image(grow_volume(mask, 2)[11,,], asp = 1,
#'       main = "Dilated (size=2) mask", axes = FALSE)
#' image(grow_volume(mask, 5)[11,,], asp = 1,
#'       main = "Dilated (size=5) mask", axes = FALSE)
#'
#'
#' @export
grow_volume <- function(volume, x, y = x, z = x, threshold = 0.5) {
  filter <- array(1.0, c(x, y, z))
  re <- convolve_volume(volume, filter)
  sel <- re > threshold
  mode(sel) <- "integer"
  sel
}
