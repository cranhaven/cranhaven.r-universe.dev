#---- A very simple single-index model
set.seed(1989)
n <- 200
x1 <- rnorm(n)
x2 <- x1 + rnorm(n)
z <- x1 + x2
y <- z + rnorm(n)
df1 <- data.frame(y, x1, x2)

# Fit model
ans <- cgaim(y ~ g(x1, x2, acons = list(monotone = 1)), 
  data = df1)

# Bootstrap
bsamp <- matrix(sample(1:n, n * 20, replace = T), n)
time1 <- system.time(boot1 <- boot.cgaim(ans, bsamples = bsamp))
# time2 <- system.time(boot2 <- boot.cgaim(ans, bsamples = bsamp, nc = 4))

# test that parallel works
# test_that("parallel works", {
#   expect_lte(time2[3], time1[3])
#   expect_equal(boot1, boot2)
# })

# test that constraints are always respected
test_that("bootstrap results are feasible", {
  expect_true(all((1 + ans$Cmat %*% boot1$boot$alpha) >= 1))
  # expect_true(all((1 + ans$alpha_control$Cmat %*% boot2$result$alpha) >= 1))
})