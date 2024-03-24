# Generate some data
y = rnorm(10)

# Specify statistic and DGP
statistic = function(x) mean(x)
dgp = function(y, v) sample(y, replace = TRUE)

# Simulate statistic N times
MaxMC:::simulation_mmc(y, statistic = statistic, dgp = dgp, v = 1, N = 99)

