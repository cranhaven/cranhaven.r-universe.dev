kernelCDF <- function(x, kernel) {
    switch(kernel,
           'gaussian' = pnorm(x),
           'epanechnikov' = epanechnikovCDF(x))
}

epanechnikovCDF <- function(x) {
    y <- ifelse(x > 1, 1, 0)
    idx <- which(abs(x) <= 1)
    y[idx] <- -0.25 * x[idx] ^ 3 + 0.75 * x[idx] + 0.5
    return (y)
}

# rectangularCDF <- function(x) {
#     y <- ifelse(x > 1, 1, 0)
#     idx <- which(abs(x) <= 1)
#     y[idx] <- 0.5 * x[idx] + 0.5
#     return (y)
# }
#
# triangularCDF <- function(x) {
#     y <- ifelse(x > 1, 1, 0)
#     idx <- which(x >= -1 & x <= 0)
#     y[idx] <- 0.5 * x[idx] ^ 2 + x[idx] + 0.5
#     idx <- which(x > 0 & x <= 1)
#     y[idx] <- -0.5 * x[idx] ^ 2 + x[idx] + 0.5
#     return (y)
# }
#
# biweightCDF <- function(x) {
#     y <- ifelse(x > 1, 1, 0)
#     idx <- which(abs(x) <= 1)
#     y[idx] <- 3/16*x[idx]^5 - 5/8*x[idx]^3 + 15/16*x[idx] + 0.5
#     return (y)
# }
#
# optcosineCDF <- function(x) {
#     y <- ifelse(x > 1, 1, 0)
#     idx <- which(abs(x) <= 1)
#     y[idx] <- 0.5*sin(0.5*pi*x[idx]) + 0.5
#     return (y)
# }
