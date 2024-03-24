# Generate some data
y = rnorm(10)

# Specify statistic and DGP
statistic = function(x) mean(x)
dgp = function(y, v) sample(y, replace = TRUE)

# Simulate statistic N times
MaxMC:::simulation_mc(y, statistic = statistic, dgp = dgp, N = 99)

