context("getFormulaInfo")

test_that("test getFormulaInfo can return the expected names of the match call.", {
  
  data = data.frame(matrix(cbind(c(1, 2, 3), c(0, 0.5, 1), c(0, -0.5, -1)), 3, 3, dimnames = list(c(), c("Y", "X1", "X2"))))
  formula = Y ~ X1 + X2
  
  namesMatchCall = getFormulaInfo(formula, data)$namesMatchCall

  expect_true(namesMatchCall[1] == "")
  expect_true(namesMatchCall[2] == "formula")
  expect_true(namesMatchCall[3] == "data")
  expect_true(is.na(namesMatchCall[4]))
  
})
