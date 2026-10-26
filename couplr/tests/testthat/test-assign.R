# Test suite for assignR modern API
# Functions are loaded from the package being tested

test_that("lap_solve() works with simple matrix input", {
  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3)
  
  result <- lap_solve(cost)
  
  expect_s3_class(result, "lap_solve_result")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_true(all(c("source", "target", "cost") %in% names(result)))
  expect_equal(attr(result, "total_cost"), 9)
})

test_that("lap_solve() handles rectangular matrices", {
  cost <- matrix(1:15, nrow = 3, ncol = 5)
  
  result <- lap_solve(cost)
  
  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 3)
  expect_true(all(result$source %in% 1:3))
  expect_true(all(result$target %in% 1:5))
})

test_that("lap_solve() respects NA masking", {
  cost <- matrix(c(4, 2, NA,
                   3, NA, 6,
                   NA, 5, 4), nrow = 3)
  
  result <- lap_solve(cost)
  
  expect_s3_class(result, "lap_solve_result")
  expect_true(all(!is.na(result$cost)))
})

test_that("lap_solve() works with maximize = TRUE", {
  profit <- matrix(c(5, 3, 7,
                     4, 6, 2,
                     8, 4, 5), nrow = 3)
  
  result <- lap_solve(profit, maximize = TRUE)
  
  expect_s3_class(result, "lap_solve_result")
  expect_gt(attr(result, "total_cost"), 10)
})

# Test lap_solve() with data frames ----

test_that("lap_solve() works with data frame input", {
  df <- tibble::tibble(
    source = rep(1:3, each = 3),
    target = rep(1:3, times = 3),
    cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
  )
  
  result <- lap_solve(df, source, target, cost)
  
  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 3)
  expect_true(all(c("source", "target", "cost") %in% names(result)))
})

test_that("lap_solve() requires all columns for data frame input", {
  df <- tibble::tibble(
    source = 1:3,
    target = 1:3
  )
  
  expect_error(
    lap_solve(df, source, target, cost),
    "must specify"
  )
})

test_that("lap_solve() works with non-sequential indices", {
  df <- tibble::tibble(
    source = c(10, 10, 10, 20, 20, 20, 30, 30, 30),
    target = c(100, 200, 300, 100, 200, 300, 100, 200, 300),
    cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
  )
  
  result <- lap_solve(df, source, target, cost)
  
  expect_equal(sort(unique(result$source)), c(10, 20, 30))
  expect_equal(sort(unique(result$target)), c(100, 200, 300))
})

# Test lap_solve() with grouped data ----

test_that("lap_solve() works with grouped data frames", {
  df <- tibble::tibble(
    sim = rep(1:2, each = 9),
    source = rep(1:3, times = 6),
    target = rep(1:3, each = 3, times = 2),
    cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4,
             1, 2, 3, 4, 3, 2, 5, 4, 1)
  )
  
  result <- df |>
    dplyr::group_by(sim) |>
    lap_solve(source, target, cost)
  
  expect_true("sim" %in% names(result))
  expect_equal(length(unique(result$sim)), 2)
  expect_equal(nrow(result), 6)  # 3 assignments per sim
})

test_that("lap_solve() preserves multiple grouping variables", {
  df <- tibble::tibble(
    scenario = rep(c("A", "B"), each = 9),
    iteration = rep(1:3, each = 6),
    source = rep(1:3, times = 6),
    target = rep(1:3, each = 3, times = 2),
    cost = runif(18, 1, 10)
  )
  
  result <- df |>
    dplyr::group_by(scenario, iteration) |>
    lap_solve(source, target, cost)
  
  expect_true(all(c("scenario", "iteration") %in% names(result)))
})

# Test lap_solve_batch() ----

test_that("lap_solve_batch() works with list of matrices", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )
  
  result <- lap_solve_batch(costs)
  
  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("problem_id" %in% names(result))
  expect_equal(length(unique(result$problem_id)), 2)
})

test_that("lap_solve_batch() works with 3D array", {
  arr <- array(c(1:8), dim = c(2, 2, 2))
  
  result <- lap_solve_batch(arr)
  
  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 2)
})

test_that("lap_solve_batch() works with grouped data frames", {
  df <- tibble::tibble(
    sim = rep(1:3, each = 4),
    source = rep(1:2, times = 6),
    target = rep(1:2, each = 2, times = 3),
    cost = runif(12, 1, 10)
  )
  
  result <- df |>
    dplyr::group_by(sim) |>
    lap_solve_batch(source, target, cost)
  
  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("sim" %in% names(result))
  expect_equal(length(unique(result$sim)), 3)
})

# Test lap_solve_kbest() ----

test_that("lap_solve_kbest() returns k solutions", {
  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3)
  
  result <- lap_solve_kbest(cost, k = 3)
  
  expect_s3_class(result, "lap_solve_kbest_result")
  expect_true("rank" %in% names(result))
  expect_true("solution_id" %in% names(result))
  expect_lte(max(result$rank), 3)
})

test_that("lap_solve_kbest() solutions are ordered by cost", {
  cost <- matrix(c(1, 2, 3,
                   4, 3, 2,
                   5, 4, 1), nrow = 3)
  
  result <- lap_solve_kbest(cost, k = 5)
  
  costs_by_rank <- result |>
    dplyr::group_by(rank, total_cost) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(rank)
  
  expect_equal(costs_by_rank$total_cost, sort(costs_by_rank$total_cost))
})

test_that("lap_solve_kbest() works with data frames", {
  df <- tibble::tibble(
    source = rep(1:3, each = 3),
    target = rep(1:3, times = 3),
    cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
  )
  
  result <- lap_solve_kbest(df, k = 3, source, target, cost)
  
  expect_s3_class(result, "lap_solve_kbest_result")
  expect_lte(max(result$rank), 3)
})

# Test utility functions ----

test_that("is_lap_solve_result() works correctly", {
  cost <- matrix(1:9, 3, 3)
  result <- lap_solve(cost)
  
  expect_true(is_lap_solve_result(result))
  expect_false(is_lap_solve_result(cost))
  expect_false(is_lap_solve_result(tibble(x = 1)))
})

test_that("get_total_cost() extracts cost correctly", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)
  
  total <- get_total_cost(result)
  expect_type(total, "double")
  expect_equal(total, attr(result, "total_cost"))
})

test_that("as_assignment_matrix() converts back to matrix", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)
  
  mat <- as_assignment_matrix(result)
  
  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(2, 2))
  expect_true(all(mat %in% c(0, 1)))
  expect_equal(sum(mat), 2)  # Should have 2 assignments
})

# Test print methods ----

test_that("print.lap_solve_result() works", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)
  
  expect_output(print(result), "Assignment Result")
  expect_output(print(result), "Total cost")
})

test_that("summary.lap_solve_kbest_result() works", {
  cost <- matrix(c(1, 2, 3,
                   4, 3, 2,
                   5, 4, 1), nrow = 3)
  result <- lap_solve_kbest(cost, k = 3)
  
  summ <- summary(result)
  
  expect_s3_class(summ, "tbl_df")
  expect_true(all(c("rank", "total_cost", "n_assignments") %in% names(summ)))
})

# Test edge cases ----

test_that("lap_solve() handles empty results gracefully", {
  cost <- matrix(NA, 2, 2)
  
  expect_error(lap_solve(cost))  # Should error on all-NA matrix
})

test_that("lap_solve() handles single element matrix", {
  cost <- matrix(5, 1, 1)
  
  result <- lap_solve(cost)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$cost[1], 5)
})

test_that("lap_solve_batch() handles empty list", {
  expect_error(lap_solve_batch(list()))
})

# Test example data ----

test_that("example_costs work correctly", {
  expect_type(example_costs, "list")
  expect_true("simple_3x3" %in% names(example_costs))
  
  result <- lap_solve(example_costs$simple_3x3)
  expect_s3_class(result, "lap_solve_result")
})

test_that("example_df works correctly", {
  expect_s3_class(example_df, "tbl_df")
  expect_true(all(c("sim", "source", "target", "cost") %in% names(example_df)))
  
  result <- example_df |>
    dplyr::group_by(sim) |>
    lap_solve(source, target, cost)
  
  expect_equal(length(unique(result$sim)), 2)
})
