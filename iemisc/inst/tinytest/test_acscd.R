test_acscd <- function() {

expect_equal(acscd(360), 0.1591551, tolerance = 1e-06)
expect_equal(acscd(180), 0.3183115, tolerance = 1e-06)
expect_equal(acscd(25), 2.292443, tolerance = 1e-06)
expect_error(acscd("sq"))

  invisible(NULL)
}

test_acscd()
