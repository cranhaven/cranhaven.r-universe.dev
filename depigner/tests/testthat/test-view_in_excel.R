test_that("view_in_excel works", {
  expect_equal(view_in_excel(mtcars), mtcars)
  expect_invisible(view_in_excel(mtcars))
})
