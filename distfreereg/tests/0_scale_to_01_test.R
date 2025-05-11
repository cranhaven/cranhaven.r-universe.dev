set.seed(20240211)

n <- 5

X <- matrix(rnorm(n^2), nrow = n)

X_scaled <- distfreereg:::scale_to_01(X)

X_scaled

all(apply(X_scaled, MARGIN = 2, min) == 0)# TRUE
all(apply(X_scaled, MARGIN = 2, max) == 1)# TRUE
