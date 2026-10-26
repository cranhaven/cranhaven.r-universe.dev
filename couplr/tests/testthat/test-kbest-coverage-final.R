# ==============================================================================
# Final coverage tests for lap_solve_kbest.R
# ==============================================================================

test_that("lap_solve_kbest errors on df without column specs", {
  skip_on_cran()
  df <- tibble::tibble(source = 1:3, target = 1:3, cost = c(1, 2, 3))
  expect_error(
    couplr::lap_solve_kbest(df, k = 2),
    "source.*target.*cost"
  )
})

test_that("lap_solve_kbest with data frame input", {
  skip_on_cran()
  df <- tibble::tibble(
    source = rep(1:2, each = 2),
    target = rep(1:2, times = 2),
    cost = c(1, 5, 5, 1)
  )

  result <- couplr::lap_solve_kbest(df, k = 2, source = source, target = target, cost = cost)
  expect_s3_class(result, "lap_solve_kbest_result")
})

test_that("print.lap_solve_kbest_result works with many solutions", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- couplr::lap_solve_kbest(cost, k = 6)  # Request 6 solutions

  # Should print "and X more solutions"
  expect_output(print(result), "K-Best Assignment")
})

test_that("summary.lap_solve_kbest_result works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- couplr::lap_solve_kbest(cost, k = 3)

  summ <- summary(result)
  expect_s3_class(summ, "tbl_df")
  expect_true("n_assignments" %in% names(summ))
})

test_that("lap_solve_kbest with maximize = TRUE", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result <- couplr::lap_solve_kbest(cost, k = 2, maximize = TRUE)
  expect_s3_class(result, "lap_solve_kbest_result")
})

test_that("kbest_assignment errors on non-numeric", {
  skip_on_cran()
  expect_error(
    couplr:::kbest_assignment(matrix(letters[1:4], 2, 2)),
    "numeric matrix"
  )
})

test_that("kbest_assignment errors on NaN", {
  skip_on_cran()
  expect_error(
    couplr:::kbest_assignment(matrix(c(1, NaN, 3, 4), 2, 2)),
    "NaN"
  )
})
