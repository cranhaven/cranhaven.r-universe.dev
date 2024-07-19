test_f4 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f4(eps = eps, D = D, Re = Re), 0.0286751684)
expect_error(f4(eps = eps, D = D, Re = 2000))
expect_error(f4(eps = 0.06, D = D, Re = Re))
expect_error(f4(eps = 0.06, Re = Re))

  invisible(NULL)
}

test_f4()
