test_that("Verify output of list_boundaries() is correct", {
  boundaries <- list_boundaries()
  
  # Do we get a character vector?
  expect_vector(boundaries, ptype = character())

  # Has data been loaded into the vector?
  expect_gt(length(boundaries), 0)

  # Are there any NA values?
  expect_in(is.na(boundaries), c(FALSE))
})
