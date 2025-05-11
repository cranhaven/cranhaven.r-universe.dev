library(distfreereg)
set.seed(20240317)
n <- 1e2
data <- data.frame(x = rnorm(n), y = rnorm(n))
data$z <- 3*data$x - 7*data$y + rnorm(n, sd = 2)
m <- lm(z ~ x + y, data = data)

dfr <- distfreereg(test_mean = m)
dfr

identical(m[["coefficients"]], dfr[["theta_hat"]])
