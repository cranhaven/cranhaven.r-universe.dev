test_that("Oz_butterflies_summary works", {
  # This really just checks that the function runs without error and returns something
  # Just read the test database
  sum <- Oz_butterflies_summary(testthat::test_path("testdata/db"))
  expect_equal(sum$Families, 5)
  expect_gte(ncol(sum), 11)
})
