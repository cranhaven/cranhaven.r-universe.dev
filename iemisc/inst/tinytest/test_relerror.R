test_relerror <- function() {

expect_equal(relerror(1.648721, 1.5), 9.02038611, tolerance = 1e-06)
expect_equal(relerror(99.9, 100), 0.1001001, tolerance = 1e-06)
expect_error(relerror(0))
expect_error(relerror("sq", 2))

  invisible(NULL)
}

test_relerror()
