test_that("preprocess_table", {
  df <- read_one_table("../data/preproc.csv")

  # Because preprocess_table() requires a spec rather than taking keyword
  # arguments, we have to make a fake one here
  mock_preproc_spec <- list(
    grouping_column = "id",
    preprocess = list(
      on = c("id", "group"),
      retain_min = "start_date",
      retain_max = "end_date"
    )
  )

  preprocessed <- preprocess_table(df, mock_preproc_spec)
  alternative <- df %>%
    group_by(id, group) %>%
    mutate(start_date = min(start_date)) %>%
    mutate(end_date = max(end_date)) %>%
    ungroup()

  expect_equal(preprocessed, alternative)
})
