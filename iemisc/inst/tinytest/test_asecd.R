test_asecd <- function() {

expect_equal(asecd(360), 89.84084, tolerance = 9e-08)
expect_equal(asecd(180), 89.68169, tolerance = 9e-08)
expect_equal(asecd(45), 88.72666, tolerance = 9e-08)
expect_equal(asecd(-1), 180)
expect_equal(asecd(1), 0)
expect_error(asecd("sq"))

  invisible(NULL)
}

test_asecd()
