y = mtcars[, c("vs", "am", "gear", "carb")]
y[, 1] = 1 + y[, 1]
y[, 2] = 1 + y[, 2]
y[, 3] = y[, 3] - 2
cuts = list(qnorm(seq(0, 1, length.out = 3)),
            qnorm(seq(0, 1, length.out = 3)),
            qnorm(seq(0, 1, length.out = 4)),
            qnorm(seq(0, 1, length.out = 9)))

rho = cor(y)
k = ncol(y)

test_that("thurstone works on parallel models", {
  lambda = rep(1, 4)
  sigma = rep(1, 4)
  expect_equal(thurstone(lambda, sigma), c(0.2, 0.2, 0.2, 0.2))
})


test_that("tr works", {
  expect_equal(tr(rho), sum(diag(rho)))
})

test_that("trim_vector accepts only numeric vectors with no NAs", {
  expect_error(trim_vector("a"))
  expect_error(trim_vector(lm))
  expect_error(trim_vector(matrix(1, 2, 2)))
  expect_error(trim_vector(c(1,NA)))
})


test_that("trim_vector accepts only numeric vectors with no NAs", {
  expect_error(trim_vector("a"))
  expect_error(trim_vector(lm))
  expect_error(trim_vector(matrix(1, 2, 2)))
  expect_error(trim_vector(c(1,NA)))
})

test_that("trim_vector removes all infinities and appends/prepends", {
  vec = c(1, Inf, -Inf, 2)
  expect_equal(trim_vector(vec)[1], -Inf)
  expect_equal(tail(trim_vector(vec),1), Inf)
  expect_equal(trim_vector(vec)[2], 1)
  expect_equal(trim_vector(vec)[3], 2)
})

test_that("massage_cuts accepts appropriate input and returns the right values", {
  expect_error(massage_cuts(NULL, "a"))
  expect_error(massage_cuts(NULL, 5), "list")
  expect_type(massage_cuts(cuts, 5), "list")
  expect_equal(length(massage_cuts(cuts[[1]])), 1)
})

test_that("xi_theoretical works", {
  rho_ = rho
  rho_[1, 1] = NA

  expect_equal(dim(xi_theoretical(cuts, rho = rho)), c(k, k))
  expect_error(xi_theoretical(cuts, rho = "a"))
  expect_error(xi_theoretical(cuts, rho = rho_))
})

test_that("xi_sample works", {
  msg = "Larger number of categories than length of cuts."
  expect_error(xi_sample(y, cuts[[1]]), regexp = msg)
  expect_equal(dim(xi_sample(y, cuts)), c(k, k))
})

test_that("x_hat throws errors and returns the correct type", {
  expect_error(x_hat(c("NA" ,1, 2), cuts[[1]]))
  expect_type(x_hat(c(2 ,1, 2), cuts[[1]]), "double")
})

test_that("standardize-functions works", {
  lambda = 1:3
  sigma = 1:3
  expect_error(standardize_lambda(c(NA ,1, 2)))
  expect_error(standardize_sigma(c(NA ,1, 2)))
  expect_equal(standardize_lambda(lambda, sigma),
               standardize_sigma(lambda, sigma))
})
