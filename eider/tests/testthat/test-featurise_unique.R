ae2_table_path <- "../data/ae2.csv"

test_that("featurise_unique", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  diag_1_unique <- featurise(
    all_tables,
    json_to_feature("../spec/test_unique.json")
  )

  # Check the result
  orig_table <- utils::read.csv(ae2_table_path)
  diag_1_unique_expected <- orig_table %>%
    group_by(id) %>%
    summarise(diag_1_unique = n_distinct(diagnosis_1)) %>%
    select(c(id, diag_1_unique))

  expect_equal(diag_1_unique$feature_table, diag_1_unique_expected)
})
