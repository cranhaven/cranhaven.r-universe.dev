test_that("Trivariate example", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  f <- x^2 + y^2 + z^2 + 2
  g <- x*y + y*z
  #
  Rz <- resultant(f, g, var = 3)
  expect_true(Rz == y^4 + 2*x^2*y^2 + 2*y^2)
  #
  Ry <- resultant(f, g, var = 2)
  expect_identical(involvedVariables(Ry), c(1L, 3L))
  #
  SRy <- principalSubresultants(f, g, var = 2)
  tests <- vapply(SRy, function(qspray) {
    !is.element(2L, involvedVariables(qspray))
  }, logical(1L))
  expect_true(all(tests))
})
