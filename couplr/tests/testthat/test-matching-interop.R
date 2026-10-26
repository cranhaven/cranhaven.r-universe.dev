# ==============================================================================
# Tests for Ecosystem Interop (as_matchit, balance_diagnostics generics)
# ==============================================================================

test_that("as_matchit works for matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  result <- match_couples(left, right, vars = "age")
  mi <- as_matchit(result, left, right)

  expect_s3_class(mi, "matchit")
  expect_true(!is.null(mi$treat))
  expect_true(!is.null(mi$weights))
  expect_true(!is.null(mi$X))
  expect_true(all(mi$treat %in% c(0, 1)))
})


test_that("as_matchit works for full_matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  result <- full_match(left, right, vars = "age")
  mi <- as_matchit(result, left, right)

  expect_s3_class(mi, "matchit")
  expect_equal(mi$method, "full")
})


test_that("as_matchit works for cem_result", {
  set.seed(42)
  left <- data.frame(id = 1:20, age = rnorm(20, 40, 10))
  right <- data.frame(id = 21:40, age = rnorm(20, 42, 10))

  result <- cem_match(left, right, vars = "age")
  mi <- as_matchit(result, left, right)

  expect_s3_class(mi, "matchit")
  expect_equal(mi$method, "cem")
})


test_that("balance_diagnostics.matching_result still works (backward compat)", {
  set.seed(123)
  left <- data.frame(
    id = 1:10,
    age = rnorm(10, 45, 10),
    income = rnorm(10, 50000, 15000)
  )
  right <- data.frame(
    id = 11:30,
    age = rnorm(20, 47, 10),
    income = rnorm(20, 52000, 15000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))

  expect_s3_class(balance, "balance_diagnostics")
  expect_true(!is.null(balance$var_stats))
  expect_true(!is.null(balance$overall))
})


test_that("balance_diagnostics.full_matching_result works", {
  set.seed(42)
  left <- data.frame(id = 1:10, age = rnorm(10, 40, 10))
  right <- data.frame(id = 11:30, age = rnorm(20, 42, 10))

  result <- full_match(left, right, vars = "age")
  balance <- balance_diagnostics(result, left, right, vars = "age")

  expect_s3_class(balance, "balance_diagnostics")
  expect_equal(balance$method, "full")
})


test_that("balance_diagnostics.cem_result works", {
  set.seed(42)
  left <- data.frame(id = 1:20, age = rnorm(20, 40, 10))
  right <- data.frame(id = 21:40, age = rnorm(20, 42, 10))

  result <- cem_match(left, right, vars = "age")
  balance <- balance_diagnostics(result, left, right, vars = "age")

  expect_s3_class(balance, "balance_diagnostics")
  expect_equal(balance$method, "cem")
})


test_that("balance_diagnostics.subclass_result works", {
  set.seed(42)
  n <- 200
  data <- data.frame(id = 1:n, x = rnorm(n))
  data$treatment <- rbinom(n, 1, plogis(data$x))

  result <- subclass_match(treatment ~ x, data, treatment = "treatment")
  balance <- balance_diagnostics(result, data = data, vars = "x")

  expect_s3_class(balance, "balance_diagnostics")
  expect_equal(balance$method, "subclassification")
})


test_that("join_matched.matching_result still works (backward compat)", {
  left <- data.frame(id = 1:5, age = c(25, 30, 35, 40, 45))
  right <- data.frame(id = 6:10, age = c(24, 29, 36, 41, 44))

  result <- match_couples(left, right, vars = "age")
  joined <- join_matched(result, left, right)

  expect_s3_class(joined, "tbl_df")
  expect_true(nrow(joined) > 0)
})


test_that("join_matched.full_matching_result works", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:15, age = runif(10, 20, 70))

  result <- full_match(left, right, vars = "age")
  joined <- join_matched(result, left, right)

  expect_s3_class(joined, "tbl_df")
  expect_true(nrow(joined) > 0)
})


test_that("join_matched.cem_result works", {
  set.seed(42)
  left <- data.frame(id = 1:20, age = rnorm(20, 40, 10))
  right <- data.frame(id = 21:40, age = rnorm(20, 42, 10))

  result <- cem_match(left, right, vars = "age")
  joined <- join_matched(result, left, right)

  expect_s3_class(joined, "tbl_df")
})


test_that("cobalt bal.tab methods require cobalt package", {
  skip_if_not_installed("cobalt")

  set.seed(42)
  left <- data.frame(id = 1:10, age = rnorm(10, 40, 10))
  right <- data.frame(id = 11:20, age = rnorm(10, 42, 10))

  result <- match_couples(left, right, vars = "age")

  # If cobalt is installed, this should work
  bt <- bal.tab.matching_result(result, left, right)
  expect_true(!is.null(bt))
})
