
test_that("confidence regions can be added", {
  depof <- test_rfield3logit(vcov = TRUE)

  expect_is(add_confregions(depof), 'field3logit')
})
