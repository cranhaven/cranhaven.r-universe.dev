test_acotd <- function() {

expect_equal(acotd(270), 0.2122056, tolerance = 1e-06)
expect_equal(acotd(45), 1.27303, tolerance = 1e-06)
expect_equal(acotd(30), 1.909152, tolerance = 1e-06)
expect_equal(acotd(1), 45)
expect_error(acotd(0))
expect_error(acotd("sq"))

  invisible(NULL)
}

test_acotd()
