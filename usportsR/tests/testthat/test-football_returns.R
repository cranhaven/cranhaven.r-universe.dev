test_that("football_returns() test" , {
  returns_2021 <- football_returns(2021)
  expect_type(returns_2021,"list")
  expect_true(nrow(returns_2021) != 0)
})
