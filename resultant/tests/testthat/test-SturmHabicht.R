test_that("SturmHabicht", {
  x <- qlone(1)
  y <- qlone(2)
  p <- x^3*y^4 + x^2*y^2 + 3*y - 6
  sh1 <- SturmHabicht(p, 1)
  sh2 <- SturmHabicht(p, 2)
  expect_length(sh1, 4L)
  expect_length(sh2, 5L)
  expect_true(sh1[[2L]] == -27*y^9 + 2*y^8*x + 54*y^8)
  expect_true(sh2[[3L]] == -8*x^8*y^2 - 36*x^6*y + 96*x^6)
})
