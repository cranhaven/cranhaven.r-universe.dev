test_that("Invalid Bandits", {
  expect_false(bandit_invalid(c(0, 0.4, 0.3)))
  expect_true(bandit_invalid(c(NA, 0.4, 0.3)))
  expect_true(bandit_invalid(c(2e-16, 2e-16, 0)))
})

test_that("Input Check Helpers", {
  test_funcs <- list(
    check_logical,
    check_prop,
    check_posint,
    check_sum1,
    check_string,
    check_names
  )
  passes <- list(
    list(TRUE, FALSE),
    list(0.5, 0.99, 0.1),
    list(5, 10, 200),
    list(c(0, 0, 1), c(0.1, 0.5, 0.4), c(0.2, 0.3, 0.5)),
    list(list(arg = "bea", valid = "bea", name = "b")),
    list(c(b = 5, c = 1), c(y = 6))
  )
  fails <- list(
    list(5, -5, NA, "b"),
    list(-1, 100, NA, "s"),
    list(-5, 0, "g", 5.5),
    list(c(1, 1, 1), c(3, 3, 2)),
    list(
      list(arg = 5, valid = "b", name = "beans"),
      list(arg = "bea", valid = "i", name = "beans")
    ),
    list(c(5, 3, 2), c("Bea", "d"))
  )
  purrr::pwalk(list(test_funcs, passes, fails), \(t, p, f) {
    input_check_test(t, p, f)
  })
})

test_that("check_clusters warns when a cluster spans multiple periods", {
  df_bad <- data.frame(
    period_number = c(1, 1, 2, 2),
    cluster = c("c1", "c1", "c1", "c2")
  )
  df_good <- data.frame(
    period_number = c(1, 1, 2, 2),
    cluster = c("c1", "c1", "c2", "c2")
  )
  expect_warning(check_clusters(df_bad, cluster_col = "cluster"))
  expect_no_warning(check_clusters(df_good, cluster_col = "cluster"))

  dt_bad <- data.table::as.data.table(df_bad)
  expect_warning(check_clusters(dt_bad, cluster_col = "cluster"))
})

test_that("check_names rejects unnamed vectors", {
  bad_vals <- list(c(1, 2, 3), list(1, 2), setNames(1, NULL))
  purrr::walk(bad_vals, \(x) {
    expect_error(check_names(v = x))
  })
  expect_no_error(check_names(v = c(a = 1, b = 2)))
})

test_that("conflict columns are properly replaced and remapped", {
  data <- data.frame(mab_condition = 1, mab_success = 2, random_col = 3)
  expect_error(col_conflict_check(data))
})
