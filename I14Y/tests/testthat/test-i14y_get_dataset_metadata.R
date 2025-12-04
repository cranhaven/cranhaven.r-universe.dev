test_that("i14y_get_dataset_metadata() returns a list", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  list <- i14y_get_dataset_metadata(
    id = "02e34f85-14df-45b5-a38b-2f063c999481"
  )
  expect_equal(class(list), "list")
  expect_true(length(list) >= 1)
})