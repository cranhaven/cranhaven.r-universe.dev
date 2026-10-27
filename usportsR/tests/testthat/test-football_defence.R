test_that("football_defence()", {
  def_2014 <- football_defence(2014)
  expect_type(def_2014,"list")
  expect_true(nrow(def_2014) != 0)
})
