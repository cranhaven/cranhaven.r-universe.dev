library(distfreereg)
set.seed(20240424)

n <- 2e1
X <- replicate(2, rbinom(n, size = 10, prob = 0.1))

am <- distfreereg:::define_aggregator_matrix(X, list(1:3))
am_ref <- cbind(
  c(           rep(1/sqrt(3), 3), rep(0,17)),
  c(rep(0, 3), rep(1/sqrt(2), 2), rep(0,15)),
  c(rep(0, 5), rep(1/sqrt(1), 1), rep(0,14)),
  c(rep(0, 6), rep(1/sqrt(3), 3), rep(0,11)),
  c(rep(0, 9), rep(1/sqrt(4), 4), rep(0, 7)),
  c(rep(0,13), rep(1/sqrt(1), 1), rep(0, 6)),
  c(rep(0,14), rep(1/sqrt(4), 4), rep(0, 2)),
  c(rep(0,18), rep(1/sqrt(1), 1), rep(0, 1)),
  c(rep(0,19), rep(1/sqrt(1), 1))
)

message('identical(am, am_ref) (should be TRUE): ', identical(am, am_ref))
