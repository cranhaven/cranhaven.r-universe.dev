context("nlist")

test_that("'nlist' is a named list", {
  expect_true(is.nlist(nlist()) && 
                is.nlist(nlist(x = 1, y = c(2,3))) && 
                is.nlist(as.nlist(list(x = 1, y = c(2,3)))))
})
