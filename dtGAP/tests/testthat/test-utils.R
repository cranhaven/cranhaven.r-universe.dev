# --- .simplify_pred -----------------------------------------------------------

test_that(".simplify_pred returns numeric vector for scalar atomic elements", {
  tab <- list("1" = 10, "2" = 20, "3" = 30)
  id <- c(1, 3)
  nam <- c("obs_a", "obs_b")

  result <- .simplify_pred(tab, id, nam)

  expect_equal(result, c(obs_a = 10, obs_b = 30))
  expect_named(result, c("obs_a", "obs_b"))
})

test_that(".simplify_pred returns factor for scalar factor elements", {
  lvls <- c("setosa", "versicolor", "virginica")
  tab <- list(
    "1" = factor("setosa", levels = lvls),
    "2" = factor("versicolor", levels = lvls),
    "3" = factor("virginica", levels = lvls)
  )
  id <- c(2, 3, 1)
  nam <- c("x", "y", "z")

  result <- .simplify_pred(tab, id, nam)

  expect_s3_class(result, "factor")
  expect_equal(levels(result), lvls)
  expect_equal(as.character(result), c("versicolor", "virginica", "setosa"))
  expect_named(result, c("x", "y", "z"))
})

test_that(".simplify_pred preserves ordered factor", {
  lvls <- c("low", "med", "high")
  tab <- list(
    "1" = ordered("low", levels = lvls),
    "2" = ordered("high", levels = lvls)
  )
  id <- c(2)
  nam <- c("obs1")

  result <- .simplify_pred(tab, id, nam)

  expect_s3_class(result, "ordered")
  expect_equal(as.character(result), "high")
})

test_that(".simplify_pred returns matrix for equal-length numeric vectors", {
  tab <- list(
    "1" = c(a = 0.2, b = 0.8),
    "2" = c(a = 0.6, b = 0.4),
    "3" = c(a = 0.9, b = 0.1)
  )
  id <- c(1, 3)
  nam <- c("obs_a", "obs_b")

  result <- .simplify_pred(tab, id, nam)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("a", "b"))
  expect_equal(rownames(result), c("obs_a", "obs_b"))
  expect_equal(result["obs_a", ], c(a = 0.2, b = 0.8))
  expect_equal(result["obs_b", ], c(a = 0.9, b = 0.1))
})

test_that(".simplify_pred returns single-row matrix with drop = FALSE", {
  tab <- list(
    "1" = c(x = 0.3, y = 0.7),
    "2" = c(x = 0.5, y = 0.5)
  )
  id <- 1
  nam <- "only"

  result <- .simplify_pred(tab, id, nam)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1)
  expect_equal(rownames(result), "only")
})

test_that(".simplify_pred falls back to list for mixed-length elements", {
  tab <- list(
    "1" = c(1, 2),
    "2" = c(3, 4, 5)
  )
  id <- c(1, 2)
  nam <- c("a", "b")

  result <- .simplify_pred(tab, id, nam)

  expect_type(result, "list")
  expect_named(result, c("a", "b"))
  expect_equal(result[["a"]], c(1, 2))
  expect_equal(result[["b"]], c(3, 4, 5))
})

test_that(".simplify_pred falls back to list for non-atomic elements", {
  tab <- list(
    "1" = list(a = 1),
    "2" = list(a = 2)
  )
  id <- c(2)
  nam <- c("obs")

  result <- .simplify_pred(tab, id, nam)

  expect_type(result, "list")
  expect_named(result, "obs")
})

# --- my_norm ------------------------------------------------------------------

test_that("my_norm maps to [0, 1]", {
  result <- my_norm(c(2, 4, 6, 8, 10))

  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(result, c(0, 0.25, 0.5, 0.75, 1))
})

test_that("my_norm handles constant input", {
  result <- my_norm(c(5, 5, 5))

  # min subtracted => all 0, then 0/0 => NaN

  expect_true(all(is.nan(result)))
})

test_that("my_norm handles single value", {
  result <- my_norm(7)

  # 7 - 7 = 0, 0 / 0 = NaN
  expect_true(is.nan(result))
})

test_that("my_norm handles NA values", {
  result <- my_norm(c(1, NA, 3, 5))

  expect_equal(result[1], 0)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 0.5)
  expect_equal(result[4], 1)
})

test_that("my_norm handles negative values", {
  result <- my_norm(c(-10, 0, 10))

  expect_equal(result, c(0, 0.5, 1))
})

# --- scale_norm (additional edge cases) ---------------------------------------

test_that("scale_norm defaults to percentize", {
  result <- scale_norm(1:5)
  expected <- scale_norm(1:5, "percentize")
  expect_equal(result, expected)
})

test_that("scale_norm handles single element", {
  expect_equal(scale_norm(5, "none"), 5)
  expect_equal(scale_norm(5, "percentize"), 1)
})

test_that("scale_norm normalize handles NA values", {
  result <- scale_norm(c(0, NA, 10), "normalize")

  expect_equal(result[1], 0)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1)
})

test_that("scale_norm scale returns numeric (not matrix)", {
  result <- scale_norm(1:5, "scale")
  expect_true(is.numeric(result))
  expect_false(is.matrix(result))
})
