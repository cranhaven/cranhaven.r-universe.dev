context("is.empty")

test_that("'is.empty' does the job", {
  expect_true(!is.empty(4) &&
                is.empty(c()) && 
                is.empty(character(0)) && 
                is.empty(list()) &&
                is.empty(integer(0)) &&
                is.empty(data.frame()))
})
