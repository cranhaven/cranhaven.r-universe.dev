lambda <- runif(5)
sigma <- runif(3)
x <- simulate_congeneric(10, k = 7, lambda = lambda, sigma = sigma)
sigma <- cov(x)

alleq <- \(x, y) isTRUE(all.equal(x, y))

alphacis <- list(
  alphaci(x, type = "adf", parallel = FALSE),
  alphaci(x, type = "elliptical", parallel = FALSE),
  alphaci(x, type = "elliptical", parallel = TRUE),
  alphaci(x, type = "normal", parallel = FALSE),
  alphaci(x, type = "normal", parallel = TRUE)
)

alphaci_fishers <- list(
  alphaci(x, type = "adf", parallel = FALSE, transform = "fisher"),
  alphaci(x, type = "elliptical", parallel = FALSE, transform = "fisher"),
  alphaci(x, type = "elliptical", parallel = TRUE, transform = "fisher"),
  alphaci(x, type = "normal", parallel = FALSE, transform = "fisher"),
  alphaci(x, type = "normal", parallel = TRUE, transform = "fisher")
)

alphaci_stds <- list(
  alphaci_std(x, type = "adf", parallel = FALSE),
  alphaci_std(x, type = "elliptical", parallel = FALSE),
  alphaci_std(x, type = "elliptical", parallel = TRUE),
  alphaci_std(x, type = "normal", parallel = FALSE),
  alphaci_std(x, type = "normal", parallel = TRUE)
)

alphaci_std_fishers <- list(
  alphaci_std(x, type = "adf", parallel = FALSE, transform = "fisher"),
  alphaci_std(x, type = "elliptical", parallel = FALSE, transform = "fisher"),
  alphaci_std(x, type = "elliptical", parallel = TRUE, transform = "fisher"),
  alphaci_std(x, type = "normal", parallel = FALSE, transform = "fisher"),
  alphaci_std(x, type = "normal", parallel = TRUE, transform = "fisher")
)

test_that("alphaci yield different results", {
  for (i in seq(length(alphacis) - 1)) {
    expect_false(alleq(alphacis[[i]], alphacis[[i + 1]]))
  }
})

test_that("alphaci fisher transform does something", {
  for (i in seq(length(alphacis) - 1)) {
    expect_false(alleq(alphacis[[i]], alphaci_fishers[[i]]))
  }
})

test_that("alphaci_std fisher transform does something", {
  for (i in seq(length(alphacis) - 1)) {
    expect_false(alleq(alphaci_stds[[i]], alphaci_std_fishers[[i]]))
  }
})

test_that("alphaci_std yield different results", {
  for (i in seq(length(alphaci_stds) - 1)) {
    expect_false(alleq(alphaci_stds[[i]], alphaci_stds[[i + 1]]))
  }
})

test_that("alphaci and alphaci_std yield different results", {
  for (i in seq(length(alphaci_stds) - 1)) {
    expect_false(alleq(alphaci_stds[[i]], alphacis[[i]]))
  }
})

test_that("alphaci bootstrap does something", {
  expect_false(alleq(
    alphacis[[1]],
    alphaci(x, type = "adf", bootstrap = TRUE, n_reps = 10)
  ))
})

test_that("alphaci_std bootstrap does something", {
  expect_false(alleq(
    alphaci_stds[[1]],
    alphaci_std(x, type = "adf", bootstrap = TRUE, n_reps = 10)
  ))
})

test_that("print is invisible", {
  capture.output(expect_invisible(print(alphaci_stds[[1]])))
})
