test_f6 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f6(eps = eps, D = D, Re = Re), 0.0288114929)
expect_equal(f6(eps = eps, Re = Re), 0.0288114929)
expect_error(f6(eps = eps, D = D, Re = 2000))
expect_error(f6(eps = 0.06, Re = Re))

  invisible(NULL)
}

test_f6()
