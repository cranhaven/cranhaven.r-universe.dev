test_that("i14y_get_content_information() returns a list", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  list <- i14y_get_content_information(
    identifier = "HCL_CH_ISCO_19_PROF"
  )
  expect_equal(class(list), "list")
  expect_true(length(list) >= 1)
})
