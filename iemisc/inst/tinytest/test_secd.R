test_secd <- function() {

expect_equal(secd(360), 1)
expect_equal(secd(180), -1)
expect_equal(secd(0), 1)
expect_error(secd("sq"))

  invisible(NULL)
}

test_secd()
