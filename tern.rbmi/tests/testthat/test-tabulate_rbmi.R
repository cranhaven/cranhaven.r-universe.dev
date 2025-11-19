data(rbmi_test_data)
dat <- rbmi_test_data
df <- tidy(dat)

# s_rbmi_lsmeans ----

testthat::test_that("s_rbmi_lsmeans works with as expected with .in_ref_col = TRUE", {
  result <- s_rbmi_lsmeans(
    df = df[1, ],
    .in_ref_col = TRUE
  )
  expected <- list(
    adj_mean_se = c(-1.6158200, 0.4862316),
    adj_mean_ci = formatters::with_label(c(-2.5757714, -0.6558685), "95% CI"),
    diff_mean_se = character(0),
    diff_mean_ci = formatters::with_label(character(0), "95% CI"),
    change = formatters::with_label(character(0), "Relative Reduction (%)"),
    p_value = character(0)
  )
  testthat::expect_equal(result, expected, tolerance = 1e-3)
})

testthat::test_that("s_rbmi_lsmeans works with as expected with .in_ref_col = FALSE", {
  result <- s_rbmi_lsmeans(
    df = df[2, ],
    .in_ref_col = FALSE
  )
  expected <- list(
    adj_mean_se = c(-1.7076264, 0.4749573),
    adj_mean_ci = formatters::with_label(c(-2.6453193, -0.7699335), "95% CI"),
    diff_mean_se = c(-0.09180645, 0.68262791),
    diff_mean_ci = formatters::with_label(c(-1.439497, 1.255884), "95% CI"),
    change = formatters::with_label(0.05681725, "Relative Reduction (%)"),
    p_value = 0.8931772
  )
  testthat::expect_equal(result, expected, tolerance = 1e-3)
})

testthat::test_that("s_rbmi_lsmeans also works with show_relative = increase", {
  result <- s_rbmi_lsmeans(
    df = df[2, ],
    .in_ref_col = FALSE,
    show_relative = "increase"
  )
  expected <- list(
    adj_mean_se = c(-1.7076264, 0.4749573),
    adj_mean_ci = formatters::with_label(c(-2.6453193, -0.7699335), "95% CI"),
    diff_mean_se = c(-0.09180645, 0.68262791),
    diff_mean_ci = formatters::with_label(c(-1.439497, 1.255884), "95% CI"),
    change = formatters::with_label(-0.05681725, "Relative Increase (%)"),
    p_value = 0.8931772
  )
  testthat::expect_equal(result, expected, tolerance = 1e-3)
})

# a_rbmi_lsmeans ----

testthat::test_that("a_rbmi_lsmeans functions as expected with valid input", {
  afun <- a_rbmi_lsmeans(df[1, ], .in_ref_col = TRUE)

  result <- unlist(afun)
  expected <- c(
    adj_mean_se1 = "-1.61581995766697",
    adj_mean_se2 = "0.486231596928312",
    adj_mean_ci1 = "-2.57577141468279",
    adj_mean_ci2 = "-0.655868500651147"
  )
  testthat::expect_equal(result, expected)
})

# summarize_rbmi ----

testthat::test_that("summarize_rbmi works as expected with valid input", {
  result <- basic_table() %>%
    split_cols_by("group", ref_group = levels(df$group)[1]) %>%
    split_rows_by("visit", split_label = "Visit", label_pos = "topleft") %>%
    summarize_rbmi() %>%
    build_table(df)

  testthat::expect_snapshot(result)
})
