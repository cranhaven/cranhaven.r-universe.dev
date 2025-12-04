test_that("i14y_get_concept() returns a list", {
  if (!curl::has_internet()) {
    skip("No internet connection")
  }
  list <- i14y_get_concept(
    id = "08d94604-e058-62a2-aa25-53f84b974201", # DV_NOGA_DIVISION
    language = "en"
  )
  expect_equal(class(list), "list")
  expect_true(length(list) >= 1)
})
