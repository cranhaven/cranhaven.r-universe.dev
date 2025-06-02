solve_quadratic <- function(a, b, c) {
  ## a * x^2 + b * x + c = 0, solve for x.
  ## only consider real roots.
  if (abs(a) < 1e-12) {
    if (abs(b) < 1e-12) {
      if (abs(c) < 1e-12) {
        flag <- "infinite"
        x <- 0
      }
      else {
        flag <- "none"
        x <- NA
      }
    }
    else {
      flag <- "one"
      x <- -c / b
    }
  }
  else {
    Delta <- b^2 - 4 * a * c
    if (Delta < 0) {
      flag <- "none"
      x <- NA
    }
    else if (Delta == 0) {
      flag <- "one"
      x <- -b / (2 * a)
    }
    else {
      flag <- "two"
      sqrt.Delta <- sqrt(Delta)
      x <- -b / (2 * a) + sqrt.Delta / (2 * a) * c(1, -1)
    }
  }
  list(flag = flag, x = x)
}
