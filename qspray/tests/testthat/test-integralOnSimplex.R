test_that("integral on simplex", {
  # variables
  x <- qlone(1); y <- qlone(2); z <- qlone(3)
  # polynomial
  P <- x^4 + y + 2*x*y^2 - 3*z
  # simplex (tetrahedron) vertices
  v1 <- c(1, 1, 1)
  v2 <- c(2, 2, 3)
  v3 <- c(3, 4, 5)
  v4 <- c(3, 2, 1)
  # simplex
  S <- rbind(v1, v2, v3, v4)
  # integral
  I <- integratePolynomialOnSimplex(P, S)
  expect_true(as.character(I) == "1387/42")
})