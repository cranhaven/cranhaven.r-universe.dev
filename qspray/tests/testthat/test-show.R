test_that("showQsprayOption", {
  set.seed(3141L)
  q1 <- rQspray()
  expect_identical(Print(q1), "-2*x^4.y^3.z^4 - 4*y^2.z^2 ")
  showQsprayOption(q1, "x") <- "A"
  expect_identical(Print(q1), "-2*A1^4.A2^3.A3^4 - 4*A2^2.A3^2 ")
})

test_that("inherited show options", {
  set.seed(3141L)
  q1 <- rQspray()
  expect_identical(Print(q1+qlone(4)), "-2*x1^4.x2^3.x3^4 - 4*x2^2.x3^2 + x4 ")
  showQsprayOption(q1, "x") <- "A"
  expect_identical(Print(q1+qlone(4)), "-2*A1^4.A2^3.A3^4 - 4*A2^2.A3^2 + A4 ")
  q2 <- rQspray()
  expect_identical(
    Print(q1 + q2), 
    "-2*A1^4.A2^3.A3^4 + 5*A1^4.A2 - A2^2.A3^3 - 4*A2^2.A3^2 - 3 "
  )
})

test_that("show univariate", {
  q3 <- 1 + qlone(1) 
  expect_identical(Print(q3), "x + 1 ")
  showQsprayOption(q3, "x") <- "A"
  expect_identical(Print(q3), "A + 1 ")
  expect_identical(Print(q3 + qlone(1)), "2*A + 1 ")
  expect_identical(Print(q3 + qlone(2)), "A1 + A2 + 1 ")
  expect_identical(Print((q3 + qlone(2)) - qlone(2)), "A + 1 ")
  showQsprayOption(q3, "showMonomial") <- showMonomialOld()
  expect_identical(Print(q3 + qlone(2)), "x^(1) + x^(0, 1) + 1 ")
  showQsprayOption(q3, "showMonomial") <- showMonomialXYZ()
  expect_identical(Print(q3 + qlone(4)), "x1 + x4 + 1 ")
})