#---- A simple two-index model
set.seed(2020)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
c1 <- rnorm(n)
mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3) + 2 * c1
y <- mu + rnorm(n)
df2 <- data.frame(y, x1, x2, x3, x4, c1)

#----- Apply model

# Fit
ans <- cgaim(y ~ 0 + g(x1, x2, acons = list(monotone = 1)) + 
    g(x3, x4, Cmat = diag(2)) + c1, data = df2)

# Check confint
ci <- confint(ans, parm = "beta")

# test
test_that("No intercept works", {
  expect_equal(ans$beta[1], 0, check.attributes = F)
  expect_equal(ci$beta[1,1], 0, check.attributes = F)
  expect_equal(ci$beta[1,2], 0, check.attributes = F)
})