test_that("simple expansion", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  expect_true((roq1 - roq2)^2 == roq1^2 - 2*roq1*roq2 + roq2^2)
})

test_that("commutativity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  expect_true(roq1*roq2*3 == 3*roq2*roq1)
  expect_true(roq1+roq2+3 == 3+roq2+roq1)
  x <- qlone(1)
  expect_true(roq1*roq2*x == x*roq2*roq1)
  expect_true(roq1+roq2+x == x+roq2+roq1)
})

test_that("associativity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(roq1*(roq2*roq3) == (roq1*roq2)*roq3)
  expect_true(roq1+(roq2+roq3) == (roq1+roq2)+roq3)
})

test_that("distributivity", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true(roq1*(roq2+roq3+3) == roq1*roq2 + roq1*roq3 + roq1*3)
})

test_that("equality", {
  roq1 <- ROQ1()
  expect_true(4*roq1/(2*roq1) == 2L)
  expect_true(4*roq1/(2*roq1) == as.character(2L))
  expect_true(4*roq1/(2*roq1) == gmp::as.bigq(2L))
  expect_true(4*roq1/(2*roq1) == as.qspray(2L))
  expect_true(4*roq1/(2*roq1) == as.ratioOfQsprays(2L))
  roq2 <- ROQ2()
  expect_true((roq1+roq2)/roq2 == roq1/roq2 + 1L)
})

test_that("division", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true((roq1/roq2) * roq3 == (roq1*roq3) / roq2)
})

test_that("power", {
  roq1 <- ROQ1()
  expect_true(roq1^7 == roq1*roq1*roq1*roq1*roq1*roq1*roq1)
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true((roq1/roq2*roq3)^3 == roq1^3/roq2^3*roq3^3)
})

test_that("negative power", {
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  expect_true((roq1 - roq2)^(-4) == 1 / (roq1 - roq2)^4)
})

test_that("arithmetic between qsprays and ratioOfQsprays", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  roq1 <- ROQ1()
  roq2 <- ROQ2()
  roq3 <- ROQ3()
  expect_true("3/2"+(x*roq1)/(y*roq2)+z == z+((x/y)*(roq1/roq2))+"3/2")
})

test_that("equality between qspray and ratioOfQsprays", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  expect_true((x^2-y^2)/(x+y) == x-y)
  expect_true((x^2-y^2)/(x+y)+z == x-y+z)
  expect_true((x^2-y^2)/(x+y)+y == x)
})

test_that("equality between scalar and ratioOfQsprays", {
  roq1 <- ROQ1()
  expect_true((3*roq1)/(roq1*6) == "1/2")
})
