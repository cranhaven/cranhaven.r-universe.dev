test_atan2d <- function() {

x <- c(1, 0, -1, 0)
y <- c(0, 1, 0, -1)

x1 <- -10.000000
y1 <- 10.000000

expect_equal(atan2d(y, x), c(0, 90, 180, -90))
expect_equal(atan2d(y1, x1), 135)
expect_error(atan2d(y1, x = "sq"))

  invisible(NULL)
}

test_atan2d()