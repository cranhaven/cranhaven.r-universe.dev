`%is%` <- expect_equal

test_that("reduce", {

  it <- icount(5)
  sum(it) %is% 15 # sum(1:5)

  it <- icount(5)
  prod(it) %is% 120 # sum(1:5)

  it <- iteror(c(1, 2, NA, 3, 4, NA, 5))
  sum(it, na.rm=TRUE) %is% 15

  it <- iteror(c(1, 2, NA, 3, 4, NA, 5))
  prod(it, na.rm=TRUE) %is% 120

  it <- icount(5)
  reduce(it, paste0, "") %is% "12345"

  it <- icount(5)
  as.character(i_accum(it, paste0, "")) %is%
    c("1", "12", "123", "1234", "12345")

})
