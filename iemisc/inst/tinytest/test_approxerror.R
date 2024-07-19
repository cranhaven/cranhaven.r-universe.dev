test_approxerror <- function() {

pres <- "ds"

prev <- 2

expect_equal(approxerror(1.5, 1), 33.3333333, tolerance = 1e-06)
expect_equal(approxerror(99.9, 100), 0.1001001, tolerance = 1e-06)
expect_error(approxerror(0))
expect_error(approxerror(pres, prev))

  invisible(NULL)
}

test_approxerror()
