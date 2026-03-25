ae2_table_path <- "../data/ae2.csv"

test_that("featurise_count", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  diag_101 <- featurise(all_tables, json_to_feature("../spec/test_count.json"))

  # Check the result
  orig_table <- utils::read.csv(ae2_table_path)
  diag_101_expected <- orig_table %>%
    filter(diagnosis_1 == 101 | diagnosis_2 == 101 | diagnosis_3 == 101) %>%
    group_by(id) %>%
    summarise(diag_101_count = n()) %>%
    select(c(id, diag_101_count))
  for (id_num in orig_table$id) {
    if (!id_num %in% diag_101_expected$id) {
      diag_101_expected <- diag_101_expected %>%
        dplyr::add_row(id = id_num, diag_101_count = 0)
    }
  }

  expect_equal(diag_101$feature_table, diag_101_expected)
})
