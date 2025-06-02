context("is.wholenumber")

test_that("'is.wholenumber' does the job", {
  expect_true(is.wholenumber(c(1L, 10L)) &&
                is.wholenumber(c(1, 10)) && 
                !is.wholenumber(1+10^(-7)) && 
                is.wholenumber(1+10^(-8)))
})
