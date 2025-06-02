quadratic.fit <- function(x, y) {
  ## Given three points on R^2, fit a quadratic curve that passes all the three points.
  ## x should be a vector of length 3, which represents the x-values of the three points.
  ## y should be a vector of length 3, which represents the y-values of the three points.
  ## output: first element: second-order coefficient
  ##         second element: first-order coefficient
  ##         third element: zero-th-order coefficient
  ##         y = a * x^2 + b * x + c.
  # library(limSolve)
  X <- cbind(x^2, x, c(1,1,1))
  if (kappa(X) > 1e8) {
    linear.reg <- lm(y ~ x)
    if (is.na(linear.reg$coefficients[2]) == TRUE) {
      linear.reg.coeff.2 <- 0
    }
    else {
      linear.reg.coeff.2 <- linear.reg$coefficients[2]
    }
    fit.coeff <- c(0, linear.reg.coeff.2, linear.reg$coefficients[1])
  }
  else {
    fit.coeff <- as.vector(Solve(X, y))
    if (fit.coeff[1] < 0) {
      linear.reg <- lm(y ~ x)
      if (is.na(linear.reg$coefficients[2]) == TRUE) {
        linear.reg.coeff.2 <- 0
      }
      else {
        linear.reg.coeff.2 <- linear.reg$coefficients[2]
      }
      fit.coeff <- c(0, linear.reg.coeff.2, linear.reg$coefficients[1])
    }
  }
  fit.coeff
}




