test_that("match_couples() memory_mode = lazy agrees with dense on the basic path", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), memory_mode = "lazy")

  expect_equal(res_lazy$pairs$left_id, res_dense$pairs$left_id)
  expect_equal(res_lazy$pairs$right_id, res_dense$pairs$right_id)
  expect_equal(res_lazy$pairs$distance, res_dense$pairs$distance, tolerance = 1e-9)
  expect_equal(res_lazy$pairs$.x_diff, res_dense$pairs$.x_diff, tolerance = 1e-9)
  expect_equal(res_lazy$pairs$.y_diff, res_dense$pairs$.y_diff, tolerance = 1e-9)
  expect_identical(res_lazy$unmatched, res_dense$unmatched)
})

test_that("match_couples() memory_mode = lazy agrees with dense under a feasible max_distance", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), max_distance = 2.5,
                             memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), max_distance = 2.5,
                            memory_mode = "lazy")

  expect_equal(sort(res_lazy$pairs$distance), sort(res_dense$pairs$distance),
              tolerance = 1e-9)
})

test_that("match_couples() memory_mode = lazy agrees with dense under a feasible caliper", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), calipers = c(x = 2.5),
                             memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), calipers = c(x = 2.5),
                            memory_mode = "lazy")

  expect_equal(sort(res_lazy$pairs$distance), sort(res_dense$pairs$distance),
              tolerance = 1e-9)
})

test_that("a caliper binds on the raw variable whatever the scaling and weights (#64)", {
  set.seed(11)
  left <- data.frame(id = paste0("L", 1:30), x = round(rnorm(30, 40, 8)),
                     y = rnorm(30, 10, 3))
  right <- data.frame(id = paste0("R", 1:60), x = round(rnorm(60, 40, 8)),
                      y = rnorm(60, 10, 3))
  setups <- list(list(scale = "standardize"), list(scale = "robust"),
                 list(weights = c(x = 4, y = 1)))
  for (setup in setups) {
    run <- function(mode) {
      suppressWarnings(do.call(match_couples, c(
        list(left, right, vars = c("x", "y"), calipers = list(x = 3),
             memory_mode = mode), setup)))
    }
    dense <- run("dense")
    raw_gap <- function(res) {
      abs(left$x[match(res$pairs$left_id, left$id)] -
            right$x[match(res$pairs$right_id, right$id)])
    }
    expect_true(all(raw_gap(dense) <= 3))
    for (mode in c("lazy", "implicit")) {
      res <- run(mode)
      expect_true(all(raw_gap(res) <= 3), info = paste(names(setup), mode))
      expect_equal(nrow(res$pairs), nrow(dense$pairs), info = paste(names(setup), mode))
      expect_equal(res$info$total_distance, dense$info$total_distance,
                   tolerance = 1e-9, info = paste(names(setup), mode))
    }
  }
})

test_that("match_couples() memory_mode = lazy recovers the dense partial matching", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  # A max_distance tight enough that no complete matching exists. The answer
  # is the largest matching the cut admits and the cheapest among those, which
  # the dense path reaches by sentinel padding and the lazy path by the design
  # loop over the same specification.
  dense <- suppressWarnings(match_couples(left, right, vars = c("x", "y"),
                                          max_distance = 1.5, memory_mode = "dense"))
  for (mode in c("lazy", "implicit")) {
    got <- suppressWarnings(match_couples(left, right, vars = c("x", "y"),
                                          max_distance = 1.5, memory_mode = mode))
    expect_lt(nrow(got$pairs), nrow(left))
    expect_equal(nrow(got$pairs), nrow(dense$pairs), info = mode)
    expect_equal(sum(got$pairs$distance), sum(dense$pairs$distance),
                 tolerance = 1e-9, info = mode)
    expect_setequal(paste(got$pairs$left_id, got$pairs$right_id),
                    paste(dense$pairs$left_id, dense$pairs$right_id))
  }
})

test_that("match_couples() solves replace and ratio over a lazy specification", {
  set.seed(8)
  left <- data.frame(id = paste0("L", 1:30), x = rnorm(30), y = rnorm(30), z = rnorm(30))
  right <- data.frame(id = paste0("R", 1:120), x = rnorm(120), y = rnorm(120),
                      z = rnorm(120))
  vars <- c("x", "y", "z")
  key <- function(res) paste(res$pairs$left_id, res$pairs$right_id)

  settings <- list(
    list(replace = TRUE),
    list(replace = TRUE, ratio = 3L),
    list(replace = TRUE, ratio = 2L, distance = "mahalanobis"),
    list(replace = TRUE, ratio = 2L, calipers = c(x = 0.3)),
    list(ratio = 2L),
    list(ratio = 3L, distance = "mahalanobis"),
    list(ratio = 2L, max_distance = 2)
  )
  for (args in settings) {
    dense <- suppressWarnings(do.call(match_couples, c(
      list(left, right, vars = vars, memory_mode = "dense"), args)))
    for (mode in c("lazy", "implicit")) {
      got <- suppressWarnings(do.call(match_couples, c(
        list(left, right, vars = vars, memory_mode = mode), args)))
      label <- paste(mode, paste(names(args), args, collapse = " "))
      expect_identical(sort(key(got)), sort(key(dense)), info = label)
      expect_equal(got$pairs$distance[order(key(got))],
                   dense$pairs$distance[order(key(dense))],
                   tolerance = 1e-9, info = label)
    }
  }
})

test_that("match_couples() still refuses greedy under memory_mode = lazy", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:6), x = rnorm(6))
  right <- data.frame(id = paste0("R", 1:6), x = rnorm(6))
  expect_error(
    match_couples(left, right, vars = "x", method = "greedy", memory_mode = "lazy"),
    "greedy"
  )
})

test_that("full_match() rejects memory_mode = lazy (different solver backend)", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:5), x = rnorm(5))
  right <- data.frame(id = paste0("R", 1:10), x = rnorm(10))

  expect_error(
    full_match(left, right, vars = "x", memory_mode = "lazy"),
    "not supported"
  )
  expect_silent(full_match(left, right, vars = "x", memory_mode = "dense"))
})
