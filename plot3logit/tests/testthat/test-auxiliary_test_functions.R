
test_that("random field3logit generator works", {
  depof <- test_rfield3logit()
  
  expect_is(depof, 'field3logit')
  expect_is(depof, 'Hfield3logit')
})
