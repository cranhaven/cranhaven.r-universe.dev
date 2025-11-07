
test_that("bind_both_table joins and relocates columns correctly", {

  df1 <- data.frame(
    VIS_Groep = "a",
    x = c("a", "b"),
    y1 = 1:2
  )

  df2 <- data.frame(
    VIS_Groep = "b",
    x = c("a", "b"),
    y2 = 3:4
  )

  df_both <- suppressMessages(bind_both_table(df1, df2, "y1", "y2"))

  #df1$VIS_Groep <- "left"

  #df_both <- bind_both_table(df1, df2, "y1", "y2")

  expect_equal(ncol(df_both), 4)
  expect_equal(unique(df_both$VIS_Groep), "a")
  expect_equal(names(df_both), c("VIS_Groep", "x", "y1", "y2"))
})
