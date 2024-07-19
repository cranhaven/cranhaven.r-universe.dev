test_f2 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f2(eps = eps, D = D, Re = Re), 0.0294431162)
expect_equal(f2(eps = eps, Re = Re), 0.0294431162)
expect_error(f2(eps = eps, D = D, Re = 2000))
expect_error(f2(eps = 0.06, Re = Re))

  invisible(NULL)
}

test_f2()
