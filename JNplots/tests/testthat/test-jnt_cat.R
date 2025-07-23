library(testthat)
library(JNplots)

# Test whether the expected intervals are found (significant interaction)
test_that("expected JN interval is found", {
  z <- jnt_cat(X='svl', Y='hl', m='species', data=microlophus, plot.full = T,
               xlab='log(SVL)', ylab='log(head length)')
  expect_equal(as.numeric(z$`lower limit in X`), 4.132542, tolerance = 0.0001)
  expect_equal(as.numeric(z$`upper limit in X`), 4.325789, tolerance = 0.0001)
})

# Test whether the output is a list
test_that("jnt_cat() returns a list", {
  z <- jnt_cat(X='svl', Y='hh', m='species', data=microlophus, plot.full = F,
               xlab='log(SVL)', ylab='log(head height)')
  expect_type(z, "list")
})
