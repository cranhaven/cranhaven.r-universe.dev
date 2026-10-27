# A feature table the package did not build, brought in as an arm of the same ladder.

flat_learner <- function() {
  learner(
    "flat",
    fit = function(x, y, ...) list(rate = colMeans(y)),
    predict = function(model, x) {
      m <- apply(x[, , 1, drop = FALSE], 1L, mean)
      outer(rank(m) / length(m), model$rate, function(a, b) a)
    })
}

test_that("a feature table becomes a one-channel array of units by features", {
  m <- matrix(seq_len(30), nrow = 10,
              dimnames = list(sprintf("p%02d", 1:10), paste0("bio", 1:3)))
  x <- feature_matrix(m)
  expect_s3_class(x, "timesift_matrix")
  expect_equal(dim(x), c(10L, 3L, 1L))
  expect_equal(dimnames(x), list(rownames(m), colnames(m), "features"))
  expect_equal(as.numeric(x[, , 1L]), as.numeric(m))
  expect_equal(attr(x, "grain"), "features")
  expect_equal(attr(x, "stats"), "features")
})

test_that("it carries no time axis, because the reduction already happened elsewhere", {
  m <- matrix(rnorm(20), nrow = 5, dimnames = list(letters[1:5], paste0("f", 1:4)))
  x <- feature_matrix(m)
  expect_true(all(is.na(attr(x, "bin_start"))))
  expect_true(all(is.na(attr(x, "bin_end"))))
  expect_true(all(is.na(attr(x, "bin_n"))))
  expect_true(is.na(attr(x, "year_start")))
  expect_false(any(attr(x, "bin_partial")))
})

test_that("the unit identifiers come from the row names or from a leading character column", {
  d <- data.frame(plot = c("a", "b", "c"), bio1 = 1:3, bio2 = 4:6, stringsAsFactors = FALSE)
  x <- feature_matrix(d)
  expect_equal(dimnames(x)[[1L]], c("a", "b", "c"))
  expect_equal(dimnames(x)[[2L]], c("bio1", "bio2"))
  expect_error(feature_matrix(list(1, 2)), "must be a matrix")
})

test_that("the label names the arm the table is reported under", {
  m <- matrix(rnorm(12), nrow = 4, dimnames = list(letters[1:4], paste0("f", 1:3)))
  x <- feature_matrix(m, label = "bioclim")
  expect_equal(dimnames(x)[[3L]], "bioclim")
  expect_equal(attr(x, "grain"), "bioclim")
})

test_that("a feature table and a calendar grain are arms of the same ladder", {
  sim <- sim_series(n_unit = 40L, days = 60L, seed = 51L)
  y <- sim_response(sim, n_var = 2L, seed = 52L)
  weekly <- grain_matrix(sim$readings, plot, t, temp, grain = "week")
  aggregates <- feature_matrix(
    cbind(level = apply(weekly[, , 1L], 1L, mean), spread = apply(weekly[, , 1L], 1L, stats::sd)),
    label = "aggregates")
  set <- timesift_set(list(week = weekly, aggregates = aggregates))
  lad <- grain_ladder(set, y, flat_learner(), folds = fold_map(y, v = 3L, seed = 5L),
                       verbose = FALSE)
  expect_setequal(unique(lad$grain), c("week", "aggregates"))
  # One mask over both arms, so the two are scored on the same cells and can be contrasted.
  by_arm <- split(lad$scorable, lad$grain)
  expect_true(identical(by_arm[[1L]], by_arm[[2L]]))
  expect_s3_class(paired_contrast(lad, "aggregates|flat", "week|flat"), "data.frame")
})
