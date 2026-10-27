test_that("tnf_universities() test", {
  universities <- tnf_universities()
  expect_type(universities, "list")
  expect_true(nrow(universities) != 0)
})
