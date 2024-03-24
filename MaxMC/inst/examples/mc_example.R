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

