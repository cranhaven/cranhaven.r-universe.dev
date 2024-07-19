test_f3 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f3(eps = eps, D = D, Re = Re), 0.0286854003)
expect_equal(f3(eps = eps, Re = Re), 0.0286854004)
expect_error(f3(eps = eps, D = D, Re = 2000))
expect_error(f3(eps = 0.06, Re = Re))

  invisible(NULL)
}

test_f3()
