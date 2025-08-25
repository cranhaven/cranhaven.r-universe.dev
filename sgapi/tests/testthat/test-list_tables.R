test_that("Verify nomis list_table function", {
  table_list = list_tables()

  # Correct column names?
  expect_setequal(colnames(table_list), c("name", "id"))

  # Are IDs unique?
  expect_equal(length(unique(table_list$id)), length(table_list$id))

  # Is there data in the data frame?
  expect_gt(dim(table_list)[1], 0)
})

