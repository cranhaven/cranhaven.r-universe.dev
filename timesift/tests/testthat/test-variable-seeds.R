test_that("a response's seed follows its name, not its place in the matrix", {
  y <- matrix(0, nrow = 2L, ncol = 3L, dimnames = list(NULL, c("a", "b", "c")))
  seeds <- .variable_seeds(1L, y)
  expect_length(seeds, 3L)
  expect_equal(length(unique(seeds)), 3L)

  reordered <- y[, c("c", "a", "b"), drop = FALSE]
  expect_equal(.variable_seeds(1L, reordered),
               seeds[match(colnames(reordered), colnames(y))])

  alone <- matrix(0, nrow = 2L, ncol = 1L, dimnames = list(NULL, "c"))
  expect_equal(.variable_seeds(1L, alone), seeds[colnames(y) == "c"])

  expect_equal(.variable_seeds(1L, y) + 9L, .variable_seeds(10L, y))
})

test_that("a response with no name falls back to its position", {
  y <- matrix(0, nrow = 2L, ncol = 2L)
  expect_equal(.variable_seeds(5L, y), c(6L, 7L))
})

# The reason the seed follows the name: a candidate whose learner covers the responses one at a
# time is fitted one column per call, so a seed read off the column's position inside its own call
# would be the same number for every response and a seed read off a shared stream would make a
# response's fit depend on the responses fitted before it.
test_that("a penalised fit of one response is the same fitted alone or beside others", {
  sim <- sim_series(n_unit = 60L, days = 60L)
  y <- sim_response(sim, n_var = 4L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")

  together <- stats::predict(fit_learner(elasticnet(), x, y), x)
  apart <- vapply(colnames(y), function(v) {
    as.numeric(stats::predict(fit_learner(elasticnet(), x, y[, v, drop = FALSE]), x))
  }, numeric(nrow(y)))
  expect_equal(unname(together[, colnames(y)]), unname(apart), tolerance = 0)

  shuffled <- y[, rev(colnames(y)), drop = FALSE]
  reordered <- stats::predict(fit_learner(elasticnet(), x, shuffled), x)
  expect_equal(unname(reordered[, colnames(y)]), unname(together[, colnames(y)]), tolerance = 0)
})

test_that("a forest fit of one response is the same fitted alone or beside others", {
  skip_if_not_installed("ranger")
  sim <- sim_series(n_unit = 60L, days = 60L)
  y <- sim_response(sim, n_var = 3L)
  x <- grain_matrix(sim$readings, plot, t, temp, grain = "week")

  together <- stats::predict(fit_learner(forest(trees = 50L), x, y), x)
  apart <- vapply(colnames(y), function(v) {
    as.numeric(stats::predict(fit_learner(forest(trees = 50L), x, y[, v, drop = FALSE]), x))
  }, numeric(nrow(y)))
  expect_equal(unname(together[, colnames(y)]), unname(apart), tolerance = 0)
})
