roll.win <- function(x, window = 3, indicator = c("mean", "var", "cov"), tau = NULL) {

  indicator = match.arg(indicator)

  if (is.vector(x) == TRUE) {

    if (length(x) < window) {
      stop("There are too few observations for this window size.", call. = F)
    }

    if (!is.null(tau) & indicator == "cov") {
      if (length(x) < window + tau) {
        stop("For this window size, parameter tau is too large.", call. = F)
      }
      if (length(x) < tau) {
        stop("Parameter tau is too large.", call. = F)
      }
    }

    n <- length(x) - window + 1
  }
  else {
    stop("x has to be a vector.", call. = F)
  }
  out <- rep(NA, n)

  if (indicator == "mean") {
    for (i in 1:n) {
      out[i] <- mean(x[i:(i+window-1)])
    }
  }

  if (indicator == "var" | (indicator == "cov" & is.null(tau))) {
    for (i in 1:n) {
      out[i] <- var(x[i:(i+window-1)])
    }
  }

  if (indicator == "cov" & !is.null(tau)) {
    n <- n - tau
    for (i in 1:n) {
      out[i] <- cov(x[i:(i+window-1)],x[(i+tau):(i+window-1+tau)])
    }
  }

  # adding NAs depending on the window width to match the original time series
  first.NAs <- rep(NA, ceiling((window - 1)/2))
  last.NAs <- rep(NA, floor((window - 1)/2))
  out <- c(first.NAs, out, last.NAs)

  return(out)

}
