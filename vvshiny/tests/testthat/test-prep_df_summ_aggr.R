test_that("prep_df_summ_aggr returns expected summarized dataframe", {

  df <- data.frame(
    #id = c(1, 1, 2, 2, 3, 3),
    split_var_value = c(
      "male",
      "male",
      "female",
      "female",
      "dutch",
      "dutch",
      "EER",
      "EER",
      "Outside EER",
      "Outside EER"
    ),
    other_var = c(
      "Early",
      "Late",
      "Early",
      "Late",
      "Early",
      "Late",
      "Early",
      "Late",
      "Early",
      "Late"
    ),
    value = c(2, 4, 6, 8, 10, 2, 4, 6, 8, 10),
    total = c(10, 10, 20, 20, 30, 30, 40, 40, 50, 50),
    split_var = c(
      "gender",
      "gender",
      "gender",
      "gender",
      "background",
      "background",
      "background",
      "background",
      "background",
      "background"
    )
  )

  expected <- data.frame(
    color = c("EER", "Outside EER", "dutch", "female", "male"),
    split_var = c("background", "background", "background", "gender", "gender"),
    value = c(5, 9, 6, 7, 3),
    Aantal = c(80, 100, 60, 40, 20)
  ) %>% dplyr::as_tibble()

  result <- prep_df_summ_aggr(df, "split_var", "value", "color", total_n_var = rlang::sym("total"), aggr_split_value_var = rlang::sym("split_var_value"))

  expect_equal(result, expected)

})
