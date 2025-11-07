test_that("prep_df_summ returns expected summary dataframe", {

  df <- data.frame(
    id = c(1, 1, 2, 2),
    group = c("A", "A", "B", "B"),
    value = c(2, 4, 6, 8)
  )

  expected <- data.frame(
    id = c(1, 2),
    group = c("A", "B"),
    value = c(3, 7),
    Aantal = c(2, 2)
  ) %>% dplyr::as_tibble()

  result <- prep_df_summ(df, c("id", "group"), "value")

  expect_equal(result, expected)

})
