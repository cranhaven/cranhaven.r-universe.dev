hanning <- function (n) {
  if (n == 1) {
    return(1)
  }
  else {
    return(0.5 - 0.5 * cos(2 * pi * seq(0, n - 1)/(n - 1)))
  }
}
