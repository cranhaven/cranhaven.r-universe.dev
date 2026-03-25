ae2_table_path <- "../data/ae2.csv"

test_that("featurise_max", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  # This is a meaningless feature, but it is a serviceable test case
  diag_101 <- featurise(
    all_tables,
    json_to_feature("../spec/test_max.json")
  )

  # Check the result
  orig_table <- utils::read.csv(ae2_table_path)
  diag_101_expected <- orig_table %>%
    group_by(id) %>%
    summarise(max_diagnosis = max(diagnosis_1)) %>%
    select(c(id, max_diagnosis))
  for (id_num in orig_table$id) {
    if (!id_num %in% diag_101_expected$id) {
      diag_101_expected <- diag_101_expected %>%
        dplyr::add_row(id = id_num, max_diagnosis = 0)
    }
  }

  expect_equal(diag_101$feature_table, diag_101_expected)
})
