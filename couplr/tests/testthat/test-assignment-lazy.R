test_that("assignment() lazy dispatch matches dense for jv and auction", {
  set.seed(42)
  left <- data.frame(id = 1:8, x = rnorm(8), y = rnorm(8))
  right <- data.frame(id = 9:20, x = rnorm(12), y = rnorm(12))
  left_mat <- as.matrix(left[, c("x", "y")])
  right_mat <- as.matrix(right[, c("x", "y")])

  spec <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  expect_s3_class(spec, "lazy_cost_spec")
  expect_equal(dim(spec), c(8L, 12L))

  dense <- compute_distance_matrix(left_mat, right_mat, distance = "euclidean")
  res_dense <- assignment(dense, method = "jv")
  res_lazy_jv <- assignment(spec, method = "jv")
  res_lazy_auto <- assignment(spec)
  res_lazy_auction <- assignment(spec, method = "auction")

  expect_equal(res_lazy_jv$total_cost, res_dense$total_cost, tolerance = 1e-9)
  expect_equal(res_lazy_auto$total_cost, res_dense$total_cost, tolerance = 1e-9)
  expect_equal(res_lazy_auction$total_cost, res_dense$total_cost, tolerance = 1e-6)
  expect_identical(res_lazy_jv$match, res_dense$match)
})

test_that("assignment() lazy dispatch handles the n_left > n_right transpose case", {
  set.seed(7)
  left <- data.frame(id = 1:12, x = rnorm(12), y = rnorm(12))
  right <- data.frame(id = 13:20, x = rnorm(8), y = rnorm(8))
  left_mat <- as.matrix(left[, c("x", "y")])
  right_mat <- as.matrix(right[, c("x", "y")])

  spec <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  dense <- compute_distance_matrix(left_mat, right_mat, distance = "euclidean")

  res_dense <- assignment(dense, method = "jv")
  res_lazy <- assignment(spec, method = "jv")

  expect_equal(res_lazy$total_cost, res_dense$total_cost, tolerance = 1e-9)
  expect_identical(res_lazy$match, res_dense$match)
})

test_that("assignment() lazy dispatch agrees with dense across metrics", {
  set.seed(11)
  left <- data.frame(id = 1:6, x = rnorm(6), y = rnorm(6), z = rnorm(6))
  right <- data.frame(id = 7:12, x = rnorm(6), y = rnorm(6), z = rnorm(6))
  left_mat <- as.matrix(left[, c("x", "y", "z")])
  right_mat <- as.matrix(right[, c("x", "y", "z")])

  for (metric in c("euclidean", "manhattan", "squared_euclidean", "chebyshev")) {
    spec <- build_cost_matrix(left, right, vars = c("x", "y", "z"),
                              distance = metric, memory_mode = "lazy")
    dense <- compute_distance_matrix(left_mat, right_mat, distance = metric)
    res_dense <- assignment(dense, method = "jv")
    res_lazy <- assignment(spec, method = "jv")
    expect_equal(res_lazy$total_cost, res_dense$total_cost, tolerance = 1e-9,
                info = metric)
  }
})

test_that("assignment() lazy dispatch agrees with dense for maximize = TRUE", {
  set.seed(13)
  left <- data.frame(id = 1:5, x = rnorm(5), y = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5), y = rnorm(5))
  left_mat <- as.matrix(left[, c("x", "y")])
  right_mat <- as.matrix(right[, c("x", "y")])

  spec <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  dense <- compute_distance_matrix(left_mat, right_mat, distance = "euclidean")

  res_dense <- assignment(dense, maximize = TRUE, method = "jv")
  res_lazy <- assignment(spec, maximize = TRUE, method = "jv")

  expect_equal(res_lazy$total_cost, res_dense$total_cost, tolerance = 1e-9)
  expect_identical(res_lazy$match, res_dense$match)
})

test_that("assignment() errors clearly for methods that don't support memory_mode = lazy", {
  set.seed(1)
  left <- data.frame(id = 1:4, x = rnorm(4))
  right <- data.frame(id = 5:8, x = rnorm(4))
  spec <- build_cost_matrix(left, right, vars = "x", memory_mode = "lazy")

  expect_error(assignment(spec, method = "hungarian"),
              "does not support memory_mode")
  expect_error(assignment(spec, method = "munkres"),
              "does not support memory_mode")
})

test_that("memory_mode = \"lazy\" states a custom distance function without calling it", {
  left <- data.frame(id = 1:4, x = rnorm(4))
  right <- data.frame(id = 5:8, x = rnorm(4))
  calls <- 0L
  custom_dist <- function(l, r) {
    calls <<- calls + 1L
    abs(outer(l[, 1], r[, 1], "-"))
  }

  spec <- build_cost_matrix(left, right, vars = "x", distance = custom_dist,
                            memory_mode = "lazy")
  expect_true(is_lazy_cost_spec(spec))
  expect_identical(spec$distance, custom_dist)
  expect_equal(calls, 0L)
})
