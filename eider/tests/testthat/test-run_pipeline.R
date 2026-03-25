test_that("run_pipeline", {
  ae2_table_path <- "../data/ae2.csv"

  all_table_filenames <- c(ae2_table_path)

  joined_feature_table <- run_pipeline(
    data_sources = list(ae2 = ae2_table_path),
    feature_filenames = c(
      "../spec/test_join1.json",
      "../spec/test_join2.json"
    )
  )

  # Check the result
  orig_table <- utils::read.csv(ae2_table_path)
  ids <- data.frame(id = sort(unique(orig_table$id)))
  diag_101_expected <- orig_table %>%
    filter(diagnosis_1 == 101 | diagnosis_2 == 101 | diagnosis_3 == 101) %>%
    group_by(id) %>%
    summarise(diag_101_count = n()) %>%
    select(c(id, diag_101_count))
  diag_102_expected <- orig_table %>%
    filter(diagnosis_1 == 102 | diagnosis_2 == 102 | diagnosis_3 == 102) %>%
    group_by(id) %>%
    summarise(diag_102_count = n()) %>%
    select(c(id, diag_102_count))
  feature_table_expected <- ids %>%
    left_join(diag_101_expected, by = "id") %>%
    mutate(diag_101_count = tidyr::replace_na(diag_101_count, 0)) %>%
    left_join(diag_102_expected, by = "id") %>%
    mutate(diag_102_count = tidyr::replace_na(diag_102_count, 0))

  # Check features
  expect_equal(
    names(joined_feature_table$features),
    c("id", "diag_101_count", "diag_102_count")
  )
  expect_equal(joined_feature_table$features$id, feature_table_expected$id)
  expect_equal(
    joined_feature_table$features$diag_101_count,
    feature_table_expected$diag_101_count
  )
  expect_equal(
    joined_feature_table$features$diag_102_count,
    feature_table_expected$diag_102_count
  )
  # Check responses
  expect_equal(names(joined_feature_table$responses), "id")
  expect_equal(joined_feature_table$responses$id, feature_table_expected$id)
})
