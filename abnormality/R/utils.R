#https://stackoverflow.com/questions/37773469/r-random-distribution-with-predefined-min-max-mean-and-sd-values
rgbeta <- function(n, mean, var, min = 0, max = 1)
{
  dmin <- mean - min
  dmax <- max - mean

  if (dmin <= 0 || dmax <= 0)
  {
    stop(paste("mean must be between min =", min, "and max =", max))
  }

  if (var >= dmin * dmax)
  {
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }

  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2

  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)

  # generate standard beta observations and transform
  x <- stats::rbeta(n, a, b)
  y <- (max - min) * x + min

  return(y)
}


# Broken-stick function
#https://alstatr.blogspot.com/2014/12/principal-component-analysis-on-imaging.html
brStick <- function (x) {
  m <- 0
  out <- matrix(NA, ncol = 2, nrow = length(x))
  colnames(out) <- c("% of Variability", "B-Stick Threshold")
  for (i in 1:length(x)) {
    for (k in i:length(x)) {
      m <- m + ((1 / length(x)) * (1 / k))
    }
    out[i, ] <- c((x[i] / sum(x)) * 100, m * 100)
    m <- 0
  }
  return(max(which(out[, 1] > out[, 2])))
}

