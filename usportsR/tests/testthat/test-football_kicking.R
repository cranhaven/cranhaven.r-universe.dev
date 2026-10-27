test_that("testing football_kicking()", {
  kicking_2017 <- football_kicking(2017)
  expect_type(kicking_2017,"list")
  expect_true(nrow(kicking_2017) != 0)
})
