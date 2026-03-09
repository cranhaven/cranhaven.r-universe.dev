
test_that("Wrong parameters in the Kummer 1F1 function", {
  expect_warning(is.nan(chf_1F1(2.5, -1, 2)))
  expect_warning(is.nan(chf_1F1(2.5, 3, 2)))
})
