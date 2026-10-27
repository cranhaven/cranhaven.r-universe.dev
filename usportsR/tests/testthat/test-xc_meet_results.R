test_that("xc_meet_results", {
  meets <- xc_meet_results()
  expect_type(meets,"list")
  expect_true(nrow(meets) != 0)
})
