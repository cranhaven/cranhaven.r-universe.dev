test_that("composeQspray", {
  x <- qlone(1)
  y <- qlone(2)
  f <- function(x, y) {
    x^4 + 2*y^3 - 4
  }
  p <- f(x, y)
  q1 <- x^2 + 1
  q2 <- y - 5
  expect_true(composeQspray(p, list(q1, q2)) == f(q1, q2))
})