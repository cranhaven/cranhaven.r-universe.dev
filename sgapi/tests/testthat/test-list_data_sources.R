test_that("returns a list",{
  expect_type(list_data_sources(), "list")
})


#test_that("Number of data sources have not changed",{
#  expect_length(list_data_sources()$source_name, 17L)
#})