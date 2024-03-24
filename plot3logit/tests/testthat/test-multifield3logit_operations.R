
test_that("summation works", {
  field1 <- test_rfield3logit()
  field2 <- test_rfield3logit()
  field3 <- field1 + field2
  
  # Class
  expect_is(field1 + field1, 'multifield3logit')
  expect_is(field3, 'multifield3logit')
  expect_is(field1 + field3, 'multifield3logit')
  expect_is(field3 + field1, 'multifield3logit')
  expect_is(field1 + field2 + field3, 'multifield3logit')
  expect_is(field3 + NULL, 'multifield3logit')
  expect_is(NULL + field3, 'multifield3logit')
  expect_is(field1 + NULL, 'multifield3logit')
  expect_is(NULL + field1, 'multifield3logit')
  
  # Length
  expect_identical(length(NULL + field1), 1L)
  expect_identical(length(field1 + field2), 2L)
  expect_identical(length(field3), 2L)
  expect_identical(length(field1 + field2 + field3), 4L)
})
