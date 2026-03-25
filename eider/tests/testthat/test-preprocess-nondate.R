test_that("preprocess_table with numbers", {
  df <- read_one_table("../data/ae2.csv")

  # Because preprocess_table() requires a spec rather than taking keyword
  # arguments, we have to make a fake one here
  mock_preproc_spec <- list(
    grouping_column = "id",
    preprocess = list(
      on = "id",
      retain_min = c("diagnosis_1", "diagnosis_2")
    )
  )

  preprocessed <- preprocess_table(df, mock_preproc_spec)
  alternative <- df %>%
    group_by(id) %>%
    mutate(diagnosis_1 = min(diagnosis_1)) %>%
    mutate(diagnosis_2 = min(diagnosis_2)) %>%
    ungroup()

  expect_equal(preprocessed, alternative)
})
