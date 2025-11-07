test_that("bind_both combines data frames correctly", {
  df1 <- data.frame(x = 1:5, y = rnorm(5), VIS_Groep_naam = "One")
  df2 <- data.frame(x = 6:10, y = rnorm(5), VIS_Groep_naam = "Two")

  df_both <- bind_both(df1, df2, id = "test",
                       y_left = "y", y_right = "y",
                       facet_var = rlang::sym("x"))

  expect_equal(nrow(df_both), 10)
  expect_equal(df_both$x, c(1:5, 6:10))
  expect_equal(mean(df_both$y[1:5]), mean(df1$y))
  expect_equal(mean(df_both$y[6:10]), mean(df2$y))
})
