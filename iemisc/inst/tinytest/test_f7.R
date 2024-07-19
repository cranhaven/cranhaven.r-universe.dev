test_f7 <- function() {

Re <- 400000
eps <- 0.004
D <- 1

expect_equal(f7(eps = eps, D = D, Re = Re), 0.0286979783)
expect_equal(f7(eps = eps, Re = Re), 0.0286450321)
expect_error(f7(eps = eps, D = D, Re = 2000))
expect_error(f7(eps = 0.06, Re = Re))

  invisible(NULL)
}

test_f7()
