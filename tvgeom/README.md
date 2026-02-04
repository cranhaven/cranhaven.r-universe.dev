# tvgeom

### Description

This R package contains PMF, CDF, quantile, and random number generation functions for the time-varying right-truncated geometric (tvgeom) distribution. The tvgeom distribution is derived from the geometric distribution and has a vector of success probabilities as its parameter. Whereas the geometric distribution has a constant probability of success over time and has no upper bound of support, the tvgeom distribution has a probability of success that changes over time. Additionally, to accommodate situations in which the event can only occur in $n$ days, after which success can not occur, the tvgeom distribution is right-truncated (it has a maximum possible value of the length, *n*, of the probability vector plus 1). When a tvgeom distributed variable has value *n+1*, this means the event did not occur in the first *n* time steps. For more detailed information on this package and the tvgeom distribution, please see the package vignette.

### Example
The following example demonstrates the relationship between the geometric distribution and the time-varying geometric distribution.
```R
library(tvgeom)
# What's the probability that a given number of trials, n, are needed to get
# one success if `prob` = `p0`, as defined below...?
p0 <- .15 # the probability of success

# Axis labels (for plotting purposes, below).
x_lab <- "Number of trials, n"
y_lab <- sprintf("P(success at trial n | prob = %s)", p0)

# Scenario 1: the probability of success is constant and we invoke functions
# from base R's implementation of the geometric distribution.
y1 <- rgeom(1e3, p0) + 1 # '+1' b/c dgeom parameterizes in terms of failures
x1 <- seq_len(max(y1))
z1 <- dgeom(x1 - 1, p0)
plot(table(y1) / 1e3,
  xlab = x_lab, ylab = y_lab, col = "#00000020",
  bty = "n", ylim = c(0, p0)
)
lines(x1, z1, type = "l")

# Scenario 2: the probability of success is constant, but we use tvgeom's
# implementation of the time-varying geometric distribution. For the purposes
# of this demonstration, the length of vector `prob` (`n_p0`) is chosen to be
# arbitrarily large *relative* to the distribution of n above (`y1`) to
# ensure we don't accidentally create any censored observations!
n_p0 <- max(y1) * 5
p0_vec <- rep(p0, n_p0)
y2 <- rtvgeom(1e3, p0_vec)
x2 <- seq_len(max(max(y1), max(y2)))
z2 <- dtvgeom(x2, p0_vec) # dtvgeom is parameterized in terms of successes
points(x2[x2 <= max(y1)], z2[x2 <= max(y1)],
  col = "red", xlim = c(1, max(y1))
)

# Scenario 3: the probability of success for each process varies over time
# (e.g., chances increase linearly by `rate` for each subsequent trial until
# chances saturate at `prob` = 1).
rate <- 1.5
prob_tv <- numeric(n_p0)
for (i in 1:length(p0_vec)) {
  prob_tv[i] <- ifelse(i == 1, p0_vec[i], rate * prob_tv[i - 1])
}
prob_tv[prob_tv > 1] <- 1
y3 <- rtvgeom(1e3, prob_tv)
x3 <- seq_len(max(y3))
z3 <- dtvgeom(x3, prob_tv)
plot(table(y3) / 1e3,
  xlab = x_lab, col = "#00000020", bty = "n",
  ylim = c(0, max(z3)),
  ylab = sprintf("P(success at trial n | prob = %s)", "`prob_tv`")
)
lines(x3, z3, type = "l")
```

### To install
```
install.packages("tvgeom")
```
or to get the latest development version
```
devtools::install_gitlab("actionable-phenology/tvgeom")
```
