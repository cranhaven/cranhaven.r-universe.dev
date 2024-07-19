test_sind <- function() {

expect_equal(sind(360), 0)
expect_equal(sind(90), 1)
expect_equal(sind(0), 0)
expect_equal(sind(180), 0)
expect_error(sind("sq"))

  invisible(NULL)
}

test_sind()