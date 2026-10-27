test_that("testing football_offence()", {
  offence_2016 <- football_offence(2016)
  expect_type (offence_2016, "list")
  expect_true(nrow(offence_2016) != 0)
})
