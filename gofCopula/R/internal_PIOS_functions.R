# internal functions and definitions for the execution of the 
# PIOSRn, PIOSTn and Kernel tests
.Power <- function(a, b) {
  return(a^b)
}
E <- exp(1)

.ksmooth2 <- function(x, v, h) {
  res.f <- sum((1 - (x[1] - v[, 1])^2 / h[1]^2)^2 * 
                 (1 - (x[2] - v[, 2])^2 / h[2]^2)^2 * 
                 ((v[, 1] - x[1])^2 <= h[1]^2) * 
                 ((v[, 2] - x[2])^2 <= h[2]^2)) / (h[1] * h[2]) * 15^2 / 
    16^2 / dim(v)[1]
  return(res.f)
}

.integrand <- function(x, Lfsample, Lbootsample, h) {
  res.f <- (.ksmooth2(x, Lfsample, h) - .ksmooth2(x, Lbootsample, h))^2
  return(res.f)
}
