test_that("xc_universities() test", {
  universities <- xc_universities()
  expect_type(universities,"list")
  expect_true(nrow(universities) != 0)
})
