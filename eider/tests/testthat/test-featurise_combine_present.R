ae2_table_path <- "../data/ae2.csv"

test_that("featurise_combine uses missing value of 0 for 'present' subfeats", {
  all_tables <- read_data(list(ae2 = ae2_table_path))
  combination <- featurise(
    all_tables,
    json_to_feature("../spec/test_combine_linear_present.json")
  )

  # Check that the output missing value is 50. The first subfeature is a
  # PRESENT subfeature, and the second one has a absent_default_value of 50, so
  # if the first subfeature is correctly being interpreted as having an
  # absent_default_value of 0, the sum should come to 50.
  expect_equal(combination$missing_value, 50)
})
