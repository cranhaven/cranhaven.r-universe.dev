test_f8 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f8(eps = eps, D = D, Re = Re), 0.0286760612)
expect_error(f8(eps = eps, D = D, Re = 2000))
expect_error(f8(eps = 0.06, D = D, Re = Re))

  invisible(NULL)
}

test_f8()
