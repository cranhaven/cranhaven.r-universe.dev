#' @title Remove the trend for one or more signals
#' @description 'Detrending' is often used before the signal power calculation.
#' @param x numerical or complex, a vector or a matrix
#' @param trend the trend of the signal; choices are \code{'constant'} and
#' \code{'linear'}
#' @param break_points integer vector, or \code{NULL}; only used when
#' \code{trend} is \code{'linear'} to remove piecewise linear trend; will
#' throw warnings if \code{trend} is \code{'constant'}
#' @returns The signals with trend removed in matrix form; the number of columns
#' is the number of signals, and number of rows is length of the signals
#' @examples
#'
#' x <- rnorm(100, mean = 1) + c(
#'   seq(0, 5, length.out = 50),
#'   seq(5, 3, length.out = 50))
#' plot(x)
#'
#' plot(detrend(x, 'constant'))
#' plot(detrend(x, 'linear'))
#' plot(detrend(x, 'linear', 50))
#'
#' @export
detrend <- function (x, trend = c("constant", "linear"), break_points = NULL) {

  trend <- match.arg(trend)

  if (!is.numeric(x) && !is.complex(x)) {
    stop("'x' must be a numeric or complex vector or matrix.")
  }

  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  n <- nrow(x)

  if(length(break_points)){
    if(trend == "constant"){
      warning("Breakpoints not used for 'constant' trend type.")
    } else {
      break_points <- as.integer(break_points)
      if(any(is.na(break_points) | break_points < 1 | break_points > n)){
        stop("'break_points' must be a subset of 1:nrow(x).")
      }
    }
  }

  if(trend == "constant"){
    x_mean <- colMeans(x, na.rm = TRUE)
    re <- sweep(x, 2, x_mean, "-", check.margin = FALSE)
    return(re)
  } else {

    break_points <- sort(unique(c(0, break_points, n - 1)))
    # print(break_points)
    nbps <- length(break_points)

    a <- sapply(seq_len(nbps), function(ii){
      if(ii == nbps){ return(rep(1, n)) }
      pad <- break_points[ii]
      m <- n - pad
      c(rep(0, pad), seq(1 / m, 1, length.out = m))
    })

    # print(a)
    # print(dim(x))
    re <- x - tcrossprod(a, t(qr.solve(a, x)))

    return(re)
  }

}
