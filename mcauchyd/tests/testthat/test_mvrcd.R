mu <- rep(0, 3)
sigma <- matrix(c(1, 0.6, 0.2, 0.6, 1, 0.3, 0.2, 0.3, 1), nrow = 3)
x <- as.matrix(rmcd(1000000, mu, sigma))
apply(x, 2, median)

scal <- numeric(nrow(x))
for (i in 1:length(scal)) {
  scal[i] <- x[i, , drop = FALSE] %*% solve(sigma) %*% t(x[i, , drop = FALSE])
}

test_that("rmcd works", {
  expect_equal(
    round(apply(x, 2, median), 2),
    mu
  )
  expect_equal(
    round(mean(log(1 + scal)), 1),
    round(digamma(0.5 + 3/2) - digamma(0.5), 1)
  )
})
