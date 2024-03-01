#### 
## perform quadrature of multivariate normal
## https://www.r-bloggers.com/notes-on-multivariate-gaussian-quadrature-with-r-code/
## compute Gauss-Hermite quadrature points and weights
## for a one-dimensional integral.
## points -- number of points
## interlim -- maximum number of Newton-Raphson iterations

hermite <- function (points, z) {
  p1 <- 1/pi^0.4
  p2 <- 0
  for (j in 1:points) {
    p3 <- p2
    p2 <- p1
    p1 <- z * sqrt(2/j) * p2 - sqrt((j - 1)/j) * p3
  }
  pp <- sqrt(2 * points) * p2
  c(p1, pp)
}

