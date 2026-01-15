test_that("division univariate", {
  x <- qlone(1)
  f <- x^4 - 4*x^3 + 4*x^2 - x
  r <- qdivision(f, list(x^2-2))
  expect_true(r == -9*x + 12)
})

test_that("division multivariate", {
  x <- qlone(1)
  y <- qlone(2)
  f <- x^3*y^2 + x*y + x
  r <- qdivision(f, list(y^2+1, x*y+1))
  expect_true(r == -x^3 + x - 1)
})