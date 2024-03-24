# MaxMC

The goal of MaxMC is to offer a straightforward and easy to use package to perform exact testing using Monte Carlo simulations in the presence of nuisance parameters.

The main functions of MaxMC are **mmc** and **mc**. The later function implements the usual Monte Carlo simulations with tie-breakers, while the former implements the Maximized Monte Carlo technique defined by Dufour (2006).

## Installation

You can install the released version of MaxMC from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MaxMC")
```

## Examples

These are basic examples which shows you how to use the **mmc** function:

``` r
## Example 1
## Exact Unit Root Test
library(fUnitRoots)

# Set seed
set.seed(123)

# Generate an AR(2) process with phi = (-1.5,0.5), and n = 25
y <- filter(rnorm(25), c(-1.5, 0.5), method = "recursive")

# Set bounds for the nuisance parameter v
lower <- -1
upper <- 1

# Set the function to generate an AR(2) integrated process
dgp <- function(y, v) {
    ran.y <- filter(rnorm(length(y)), c(1-v,v), method = "recursive")
}

# Set the Augmented-Dicky Fuller statistic
statistic <- function(y){
    out <- suppressWarnings(adfTest(y, lags = 2, type = "nc"))
    return(out@test$statistic)
}

# Apply the mmc procedure
mmc(y, statistic = statistic , dgp = dgp, lower = lower,
    upper = upper, N = 99, type = "leq", method = "GenSA",
    control = list(max.time = 2))


## Example 2
## Behrens-Fisher Problem
library(MASS)

# Set seed
set.seed(123)

# Generate sample x1 ~ N(0,1) and x2 ~ N(0,4)
x1 <- rnorm(15, mean = 0, sd = 1)
x2 <- rnorm(25, mean = 0, sd = 2)
data <- list(x1 = x1, x2 = x2)

# Fit a normal distribution on x1 and x2 using maximum likelihood
fit1 <- fitdistr(x1, "normal")
fit2 <- fitdistr(x2, "normal")

# Extract the estimate for the nuisance parameters v = (sd_1, sd_2)
est <- c(fit1$estimate["sd"], fit2$estimate["sd"])

# Set the bounds of the nuisance parameters equal to the 99% CI
lower <- est - 2.577 * c(fit2$sd["sd"], fit1$sd["sd"])
upper <- est + 2.577 * c(fit2$sd["sd"], fit1$sd["sd"])

# Set the function for the DGP under the null (i.e. two population means are equal)
dgp <- function(data, v) {
    x1 <- rnorm(length(data$x1), mean = 0, sd = v[1])
    x2 <- rnorm(length(data$x2), mean = 0, sd = v[2])
    return(list(x1 = x1, x2 = x2))
}

# Set the statistic function to Welch's t-test
welch <- function(data) {
    test <- t.test(data$x2, data$x1)
    return(test$statistic)
}

# Apply Welch's t-test
t.test(data$x2, data$x1)

# Apply the mmc procedure
mmc(y = data, statistic = welch, dgp = dgp, est = est,
    lower = lower, upper = upper, N = 99,	type = "absolute",
    method = "pso")
```


This is a basic example which shows you how to use the **mc** function:

``` r
## Example 1
## Kolmogorov-Smirnov Test using Monte Carlo

# Set seed
set.seed(999)

# Generate sample data
y <- rgamma(8, shape = 2, rate = 1)

# Set data generating process function
dgp <- function(y)  rgamma(length(y), shape = 2, rate = 1)

# Set the statistic function to the Kolomogorov-Smirnov test for gamma distribution
statistic <- function(y){
    out <- ks.test(y, "pgamma", shape = 2, rate = 1)
    return(out$statistic)
}

# Apply the Monte Carlo test with tie-breaker
mc(y, statistic = statistic, dgp = dgp, N = 999, type = "two-tailed")
```

## References
Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters: A general approach to finite sample inference and nonstandard asymptotics in econometrics. *Journal of Econometrics*, **133(2)**, 443-447.
