ae2_table_path <- "../data/ae2.csv"

test_that("join_feature_tables (with all_ids specified)", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  all_ids <- 1:200

  diag_101 <- featurise(
    all_tables,
    json_to_feature("../spec/test_join1.json")
  )
  diag_102 <- featurise(
    all_tables,
    json_to_feature("../spec/test_join2.json")
  )

  # Join the feature tables
  joined_feature_table <- join_feature_tables(
    list(diag_101, diag_102),
    all_ids = all_ids
  )

  # Check the result
  orig_table <- utils::read.csv(ae2_table_path)
  ids <- data.frame(id = all_ids)
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

  expect_equal(joined_feature_table$features$id, feature_table_expected$id)
  expect_equal(
    joined_feature_table$features$diag_101_count,
    feature_table_expected$diag_101_count
  )
  expect_equal(
    joined_feature_table$features$diag_102_count,
    feature_table_expected$diag_102_count
  )
})
