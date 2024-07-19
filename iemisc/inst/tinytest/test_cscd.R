test_cscd <- function() {

expect_equal(cscd(30), 2)
expect_equal(cscd(1), 57.29869, tolerance = 1e-06)
expect_equal(cscd(45), 1.414214, tolerance = 1e-06)
expect_error(cscd("sq"))

  invisible(NULL)
}

test_cscd()
