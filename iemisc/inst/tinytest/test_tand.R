test_tand <- function() {

expect_equal(tand(360), 0)
expect_equal(tand(0), 0)
expect_equal(tand(180), 0)
expect_error(tand("sq"))

  invisible(NULL)
}

test_tand()