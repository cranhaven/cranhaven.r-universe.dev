test_that("display_name returns expected result", {

  # Custom mapping
  mapping <- list(
    col1 = "Column 1",
    col2 = "Column 2"
  )

  expect_equal(display_name("col1", mapping), "Column 1")
  expect_equal(display_name("col2", mapping), "Column 2")

  # Non-mapped values
  expect_equal(display_name("col3", mapping), "col3")

})
