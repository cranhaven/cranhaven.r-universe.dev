# The designs match_couples() offers, compiled into the flow model.
#
# What is checked here is the compiled design and the maps it returns, and then
# that the answers read back through those maps are the ones the designs are
# defined to give: the assignment optimum for 1:1, the replicated assignment
# optimum for k:1, and each row's own cheapest columns for replacement.

test_that("the 1:1 design compiles to the assignment R already solves", {
  plan <- couplr:::.couples_design(3, 5)

  expect_identical(plan$design, "one_to_one")
  expect_identical(plan$route, "assignment")
  expect_false(plan$reshaped)
  expect_identical(plan$row_unit, 1:3)
  expect_identical(plan$col_unit, 1:5)
  expect_equal(plan$per_row, 1)
  expect_equal(plan$flow_required, 3)
})

test_that("the k:1 design compiles to the same assignment with replicated rows", {
  plan <- couplr:::.couples_design(3, 5, ratio = 2L)

  expect_identical(plan$design, "fixed_ratio")
  expect_identical(plan$route, "assignment")
  expect_true(plan$reshaped)
  expect_identical(plan$row_unit, rep(1:3, each = 2L))
  expect_identical(plan$col_unit, 1:5)
  expect_equal(plan$flow_required, 6)
})

test_that("the replacement design separates by row and states each row's quota", {
  plan <- couplr:::.couples_design(3, 5, replace = TRUE, ratio = 2L)

  expect_identical(plan$design, "with_replacement")
  expect_identical(plan$route, "separable")
  expect_false(plan$reshaped)
  expect_equal(plan$per_row, 2)
  expect_equal(plan$flow_required, 6)

  # A row cannot take more distinct columns than exist.
  narrow <- couplr:::.couples_design(3, 1, replace = TRUE, ratio = 4L)
  expect_equal(narrow$per_row, 1)
  expect_equal(narrow$flow_required, 3)
})

test_that("the compiled maps are what the cost matrix is read through", {
  cost <- matrix(c(1, 2, 3,
                   4, 5, 6), nrow = 2, byrow = TRUE)

  one_to_one <- couplr:::.couples_design(2, 3)
  expect_identical(couplr:::.couples_costs(cost, one_to_one), cost)

  k_to_one <- couplr:::.couples_design(2, 3, ratio = 2L)
  expect_identical(couplr:::.couples_costs(cost, k_to_one),
                   cost[c(1, 1, 2, 2), , drop = FALSE])
})

test_that("a design with no compiler is an error, not a route", {
  expect_error(couplr:::lap_flow_compile_couples("subclassification", 3, 3, 1),
               "no compiler")
  expect_error(couplr:::lap_flow_compile_couples("one_to_one", -1, 3, 1),
               "negative")
  expect_error(couplr:::lap_flow_compile_couples("fixed_ratio", 3, 3, 1.5),
               "whole number")
  expect_error(couplr:::lap_flow_compile_couples("fixed_ratio", 3, 3, 0),
               "ratio must be at least 1")
})

test_that("k:1 matching gives every left unit its ratio of right units", {
  set.seed(11)
  left <- data.frame(id = 1:4, x = rnorm(4))
  right <- data.frame(id = 5:16, x = rnorm(12))

  result <- match_couples(left, right, vars = "x", ratio = 2L,
                          check_costs = FALSE, return_diagnostics = TRUE)

  expect_equal(nrow(result$pairs), 8)
  expect_true(all(table(result$pairs$left_id) == 2L))
  expect_equal(length(unique(result$pairs$right_id)), 8)
  expect_equal(result$info$ratio, 2L)

  # The k:1 optimum is the optimum of the replicated assignment, which is what
  # the design compiles to.
  costs <- as.matrix(dist(rbind(left["x"], right["x"])))[1:4, 5:16]
  replicated <- costs[rep(seq_len(4), each = 2), , drop = FALSE]
  expect_equal(sum(result$pairs$distance),
               assignment(replicated)$total_cost)
})

test_that("matching with replacement gives each row its own cheapest columns", {
  set.seed(12)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:13, x = rnorm(8))

  result <- match_couples(left, right, vars = "x", replace = TRUE, ratio = 3L,
                          check_costs = FALSE, return_diagnostics = TRUE)

  expect_equal(nrow(result$pairs), 15)
  expect_true(result$info$replace)

  costs <- as.matrix(dist(rbind(left["x"], right["x"])))[1:5, 6:13]
  own_best <- sum(apply(costs, 1, function(row) sum(sort(row)[1:3])))
  expect_equal(sum(result$pairs$distance), own_best)
})

test_that("a lazy cost source stays whole on the design that does not reshape it", {
  set.seed(13)
  left <- data.frame(id = 1:30, x = rnorm(30))
  right <- data.frame(id = 31:90, x = rnorm(60))

  lazy <- match_couples(left, right, vars = "x", memory_mode = "lazy",
                        check_costs = FALSE)
  dense <- match_couples(left, right, vars = "x", memory_mode = "dense",
                         check_costs = FALSE)

  expect_equal(sum(lazy$pairs$distance), sum(dense$pairs$distance))

  # The designs that reshape their input read the specification through the
  # design's maps rather than a matrix, and reach the dense answer.
  for (args in list(list(ratio = 2L), list(replace = TRUE))) {
    lazy_k <- do.call(match_couples, c(list(left, right, vars = "x",
                                            memory_mode = "lazy",
                                            check_costs = FALSE), args))
    dense_k <- do.call(match_couples, c(list(left, right, vars = "x",
                                             memory_mode = "dense",
                                             check_costs = FALSE), args))
    expect_equal(sum(lazy_k$pairs$distance), sum(dense_k$pairs$distance))
    expect_equal(nrow(lazy_k$pairs), nrow(dense_k$pairs))
  }
})
