test_Re3 <- function() {

D <- "s"

G <- 2

mu <- 4

expect_equal(Re3(D = 100, G = 6236.37035, mu = 2.34143326e-05, units = "Eng"), 3391253.33225764, tolerance = 9e-08)
expect_error(Re3(c(D = D, G = G, mu = mu, units = "Eng")))

  invisible(NULL)
}

test_Re3()
