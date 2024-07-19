test_cv <- function() {

x <- c(14.1, 121, 212, 28, 330, 63, 29, 63, 55, 19, 20)

expect_equal(cv(x), 114.8747, tolerance = 1e-06)
expect_equal(cv(c(99, 200)), 47.77109, tolerance = 1e-06)
expect_error(cv(0))
expect_error(cv(1))
expect_error(cv(1003.23))
expect_error(cv(99))
expect_error(cv("50"))
expect_error(cv("sq"))

  invisible(NULL)
}

test_cv()
