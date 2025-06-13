#' @param  x vector or matrix
#' @param n digits
#' @author Chai
#' @title Round of  Numbers that is improved.
#' @description Round : rounding off to five in double.
#' round_1 and Round2: to achieve the standard sense of rounding.
#' @return  It rounds the values and output to console.
#' @export
Round <- function(x, n = 0) {
  n <- trunc(n)
  x <- x * 10^n
  which(x %% 2 == 0.5) -> q1
  floor(x[q1]) -> q2
  x[q1] <- NA
  round_1(x, 0) -> x2
  x2[q1] <- q2
  x2 / 10^n
}

#' @rdname  Round
#' @export
round_1 <- function(x, n) {
  a1 <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^n
  z * a1
}

#' @rdname  Round
#' @export
Round2 <- function(x, n) {
  x1 <- abs(x) * 10^n
  a1 <- sign(x)
  which(x1 %% 1 >= 0.5) -> q1
  floor(x1[q1]) + 1 -> q2
  x2 <- round_1(x1, 0)
  x2[q1] <- q2
  x2 / 10^n * a1
}
