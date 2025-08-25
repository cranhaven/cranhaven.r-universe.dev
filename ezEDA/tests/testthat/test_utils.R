d <- data.frame(x = c(1, 2, 3, 4), y = c(10, 20, 30, 40))
tst <- function(x) {
   col <- rlang::enquo(x)
   x_col <- rlang::enquo(x)
   d <- col_to_factor(d, x_col)
   d
}
w <- tst(x)
test_that("The first column has been converted to factor",{
  expect_is(w$x, "factor")
})
