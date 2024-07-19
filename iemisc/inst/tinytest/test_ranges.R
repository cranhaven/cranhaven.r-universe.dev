test_ranges <- function() {

rando::set_n(100) # makes the example reproducible
x <- rando::r_norm(.seed = 943)

xi <- c(NA, 1:3, -1:1/0)

expect_equal(ranges(x), 4.889942, tolerance = 1e-06)
expect_error(ranges(ranges(xi)))
expect_error(ranges(xi, na.rm = TRUE))
expect_error(ranges(xi, finite = TRUE))

  invisible(NULL)
}

test_ranges()
