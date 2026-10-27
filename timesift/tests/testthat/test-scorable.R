test_that("a cell needs both classes on both sides of the split", {
  units <- sprintf("p%02d", 1:10)
  folds <- stats::setNames(rep(1:5, each = 2L), units)
  y <- matrix(0, nrow = 10, ncol = 4, dimnames = list(units, c("all", "none", "one", "split")))
  y[, "all"] <- 1
  y[, "none"] <- 0
  y[1, "one"] <- 1                    # the single presence sits in fold 1
  y[c(1, 3, 5, 7), "split"] <- 1      # presences in folds 1, 2, 3, 4

  cells <- scorable_cells(y, folds)
  get <- function(v, k) cells$scorable[cells$variable == v & cells$fold == k]

  expect_false(any(cells$scorable[cells$variable == "all"]))
  expect_false(any(cells$scorable[cells$variable == "none"]))
  expect_false(get("one", 1L))        # nothing is left to fit on
  expect_false(get("one", 2L))        # nothing is left to score on
  expect_true(get("split", 1L))
  expect_false(get("split", 5L))      # fold 5 holds no presence
})

test_that("the counts on each side of the split add up to the whole", {
  sim <- sim_series(n_unit = 90L, days = 2L)
  y <- sim_response(sim, n_var = 4L)
  cells <- scorable_cells(y, fold_map(y, v = 6L))
  expect_equal(cells$pres_train + cells$pres_test, cells$n_occ)
  expect_equal(cells$pres_train + cells$pres_test + cells$abs_train + cells$abs_test,
               rep(nrow(y), nrow(cells)))
})

test_that("the mask sees the response and the fold map and nothing else", {
  sim <- sim_series(n_unit = 60L, days = 2L)
  y <- sim_response(sim, n_var = 3L)
  f <- fold_map(y, v = 5L)
  expect_identical(scorable_cells(y, f), scorable_cells(y[nrow(y):1, , drop = FALSE], f))
})

test_that("a response arrives as a matrix, a data frame with identifiers, or a vector", {
  units <- sprintf("p%02d", 1:6)
  m <- matrix(c(0, 1, 0, 1, 1, 0), ncol = 1, dimnames = list(units, "sp1"))
  d <- data.frame(logger = units, sp1 = c(0, 1, 0, 1, 1, 0), stringsAsFactors = FALSE)
  expect_equal(.as_response(d), m)
  expect_equal(.as_response(stats::setNames(c(0, 1, 0, 1, 1, 0), units)),
               matrix(c(0, 1, 0, 1, 1, 0), ncol = 1, dimnames = list(units, "y")))
  expect_error(.as_response(data.frame(id = units, sp1 = c(0, 1, NA, 1, 1, 0))), "missing values")
  expect_error(.as_response(data.frame(id = units, site = units, sp1 = c(0, 1, 0, 1, 1, 0))),
               "2 non-numeric columns (id, site)", fixed = TRUE)
})

test_that("a presence-absence response must actually be presence-absence", {
  units <- sprintf("p%02d", 1:4)
  y <- matrix(c(0, 1, 2, 1), ncol = 1, dimnames = list(units, "sp1"))
  expect_error(.presence_absence$prepare(y), "0/1 or logical")
})
