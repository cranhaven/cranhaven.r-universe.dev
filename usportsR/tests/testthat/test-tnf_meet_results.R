test_that("tnf_meet_results() test", {
  meets <- tnf_meet_results()
  expect_type(meets,"list")
  expect_true(nrow(meets) != 0)
})
