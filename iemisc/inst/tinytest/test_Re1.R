test_Re1 <- function() {

expect_equal(Re1(D = 100, V = 100, rho = 62.3637035, mu = 2.34143326e-05, units = "Eng"), list(nu = 1.207966e-05, Re1 = 827837834), tolerance = 9e-08)
expect_error(Re1("sq"))
expect_error(Re1(NA))

  invisible(NULL)
}

test_Re1()
