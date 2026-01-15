test_that("substituteQspray", {
  x1 <- qlone(1)
  x2 <- qlone(2)
  x3 <- qlone(3)
  x4 <- qlone(4)
  f <- x1^2 + x3^2 + x1*x2*x3 + x1*x2 - x4 - 1
  g <- substituteQspray(f, c("2", "3/2", NA, NA))
  expect_true(g == x3^2 + 3*x3 - x4 + 6)
})