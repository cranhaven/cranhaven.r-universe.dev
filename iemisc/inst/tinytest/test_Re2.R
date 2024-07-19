test_Re2 <- function() {


expect_equal(Re2(D = 100, V = 100, nu = 1.20796605e-05), 827837835)
expect_error(Re2("sq"))
expect_error(Re2(NA))

  invisible(NULL)
}

test_Re2()
