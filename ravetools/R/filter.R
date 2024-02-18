#' @title Filter one-dimensional signal
#' @description The function is written from the scratch. The result has been
#' compared against the 'Matlab' \code{filter} function with one-dimensional
#' real inputs. Other situations such as matrix \code{b} or multi-dimensional
#' \code{x} are not implemented.
#' @param b one-dimensional real numerical vector, the moving-average
#' coefficients of an \code{ARMA} filter
#' @param a the auto-regressive (recursive) coefficients of an \code{ARMA} filter
#' @param x numerical vector input (real value)
#' @param z initial condition, must have length of \code{n-1}, where \code{n}
#' is the maximum of lengths of \code{a} and \code{b}; default is all zeros
#' @returns A list of two vectors: the first vector is the filtered signal;
#' the second vector is the final state of \code{z}
#'
#' @examples
#'
#'
#' t <- seq(0, 1, by = 0.01)
#' x <- sin(2 * pi * t * 2.3)
#' bf <- signal::butter(2, c(0.15, 0.3))
#'
#' res <- filter_signal(bf$b, bf$a, x)
#' y <- res[[1]]
#' z <- res[[2]]
#'
#' ## Matlab (2022a) equivalent:
#' # t = [0:0.01:1];
#' # x = sin(2 * pi * t * 2.3);
#' # [b,a] = butter(2,[.15,.3]);
#' # [y,z] = filter(b, a, x)
#'
#'
#' @export
filter_signal <- function(b, a, x, z) {

  na <- length(a)
  nb <- length(b)

  if( na > nb ) {
    b <- c(b, rep(0, na - nb))
    n <- na
  } else {
    a <- c(a, rep(0, nb - na))
    n <- nb
  }

  if(missing(z)) {
    z <- rep(0.0, n - 1)
  } else {
    if(length(z) < n-1) {
      stop(sprintf("`filter`: initial condition `z` must have length >= %d", n-1))
    }
  }

  if(!is.double(a)) {
    a <- as.double(a)
  }
  if(!is.double(b)) {
    b <- as.double(b)
  }
  if(!is.double(z)) {
    z <- as.double(z)
  }
  if(!is.double(x)) {
    x <- as.double(x)
  }
  return(cpp_filter(b, a, x, z))
}
