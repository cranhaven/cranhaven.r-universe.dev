set.seed(14)

#Application to dataset seeds
data("IMDb")
X <- IMDb$Y


n <- dim(X)[3]
p <- dim(X)[1]
T <- dim(X)[2]
