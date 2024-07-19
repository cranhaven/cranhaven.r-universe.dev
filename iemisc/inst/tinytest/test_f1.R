test_f1 <- function() {

expect_equal(f1(2000), 0.032)
expect_equal(f1(0.00001), 6400000)
expect_error(f1("sq"))
expect_error(f1(2500))

  invisible(NULL)
}

test_f1()
