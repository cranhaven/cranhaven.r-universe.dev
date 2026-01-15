test_that("simple expansion", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  expect_true(Print((roq1 - roq2)^2) == Print(roq1^2 - 2*roq1*roq2 + roq2^2))
})

test_that("commutativity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  expect_true(Print(roq1*roq2*3) == Print(3*roq2*roq1))
  x <- qlone(1)
  expect_true(Print(roq1*roq2*x) == Print(x*roq2*roq1))
})

test_that("associativity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(Print(roq1*(roq2*roq3)) == Print((roq1*roq2)*roq3))
})

test_that("distributivity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(
    Print(roq1*(roq2+roq3+3)) == Print(roq1*roq2 + roq1*roq3 + roq1*3)
  )
})

test_that("equality with scalar", {
  roq1 <- ROQ1()
  expect_true(Print(4*roq1/(2*roq1)) == "[ 2 ] ")
  expect_true(Print(2*roq1/(4*roq1)) == "[ 1/2 ] ")
})

test_that("division", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(Print((roq1/roq2) * roq3) == Print((roq1*roq3) / roq2))
})

test_that("power", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(Print((roq1/roq2*roq3)^3) == Print(roq1^3/roq2^3*roq3^3))
})

test_that("arithmetic between qsprays and ratioOfQsprays", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(
    Print("3/2"+(x*roq1)/(y*roq2)+z) == Print(z+((x/y)*(roq1/roq2))+"3/2")
  )
})

test_that("equality between qspray and ratioOfQsprays", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  expect_true(Print((x^2-y^2)/(x+y)) == "[ x - y ] ")
})

test_that("equality between scalar and ratioOfQsprays", {
  roq1 <- ROQ1()
  expect_true(Print((3*roq1)/(roq1*6)) == "[ 1/2 ] ")
})

test_that("show is inherited from qspray", {
  set.seed(3141)
  q1 <- rQspray()
  q2 <- rQspray()
  expect_identical(
    Print(q1 / q2),
    "[ -2/5*x^4.y^3.z^4 - 4/5*y^2.z^2 ]  %//%  [ x^4.y - 1/5*y^2.z^3 - 3/5 ] "
  )
  showQsprayOption(q1, "x") <- "A"
  expect_identical(
    Print(q1 / q2),
    "[ -2/5*A1^4.A2^3.A3^4 - 4/5*A2^2.A3^2 ]  %//%  [ A1^4.A2 - 1/5*A2^2.A3^3 - 3/5 ] "
  )
  q1 <- qlone(1)
  showQsprayOption(q1, "x") <- "A"
  expect_identical(
    Print(q1 / (1 + q1)),
    "[ A ]  %//%  [ A + 1 ] "
  )
  expect_identical(
    Print(q1 / q2),
    "[ 1/5*A1 ]  %//%  [ A1^4.A2 - 1/5*A2^2.A3^3 - 3/5 ] "
  )
})

test_that("show options are inherited by the first operand", {
  set.seed(3141L)
  q1 <- rQspray()
  q2 <- rQspray()
  roq1 <- q1 / q2
  # slow: roq1 + (q2/q1)
  # num <- roq1@numerator*q1 + q2*roq1@denominator
  # den <- q1*roq1@denominator
  # slow: resultant::gcd(num, den)
  expect_identical(
    Print(roq1+qlone(4)),
    "[ -2/5*x1^4.x2^3.x3^4 + x1^4.x2.x4 - 1/5*x2^2.x3^3.x4 - 4/5*x2^2.x3^2 - 3/5*x4 ]  %//%  [ x1^4.x2 - 1/5*x2^2.x3^3 - 3/5 ] "
  )
  showRatioOfQspraysOption(roq1, "x") <- "A"
  expect_identical(
    Print(roq1),
    "[ -2/5*A1^4.A2^3.A3^4 - 4/5*A2^2.A3^2 ]  %//%  [ A1^4.A2 - 1/5*A2^2.A3^3 - 3/5 ] "
  )
  expect_identical(
    Print(roq1 * (q1/q2)),
    "[ 4/25*A1^8.A2^6.A3^8 + 16/25*A1^4.A2^5.A3^6 + 16/25*A2^4.A3^4 ]  %//%  [ A1^8.A2^2 - 2/5*A1^4.A2^3.A3^3 - 6/5*A1^4.A2 + 1/25*A2^4.A3^6 + 6/25*A2^2.A3^3 + 9/25 ] "
  )

  roq <- qlone(1) / (1 + qlone(1))
  expect_identical(Print(roq), "[ x ]  %//%  [ x + 1 ] ")
  showRatioOfQspraysOption(roq, "x") <- "A"
  expect_identical(Print(roq), "[ A ]  %//%  [ A + 1 ] ")
  expect_identical(Print(roq + qlone(1)), "[ A^2 + 2*A ]  %//%  [ A + 1 ] ")
  expect_identical(Print(roq + qlone(2)), "[ A1.A2 + A1 + A2 ]  %//%  [ A1 + 1 ] ")
})

test_that("show numerator and show denominator are in agreement", {
  roq <- (qlone(1) + qlone(2)) / (qlone(3) + qlone(4))
  expect_identical(Print(roq), "[ x1 + x2 ]  %//%  [ x3 + x4 ] ")
  expect_identical(Print(getNumerator(roq)), "x1 + x2 ")
  expect_identical(Print(getDenominator(roq)), "x3 + x4 ")
})
