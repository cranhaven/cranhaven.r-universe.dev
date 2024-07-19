test_atand <- function() {

expect_equal(atand(360), 89.84085, tolerance = 9e-08)
expect_equal(atand(90), 89.36341, tolerance = 9e-08)
expect_equal(atand(180), 89.68169, tolerance = 9e-08)
expect_equal(atand(0), 0)
expect_equal(atand(1), 45)
expect_error(atand("sq"))

  invisible(NULL)
}

test_atand()
