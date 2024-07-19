test_asind <- function() {

expect_equal(asind(1), 90)
expect_equal(asind(0), 0)
expect_error(asind("sq"))

  invisible(NULL)
}

test_asind()
