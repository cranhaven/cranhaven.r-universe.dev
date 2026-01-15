test_that("getCoefficient and getConstantTerm", {
  qspray <- qlone(2)
  expect_true(getCoefficient(qspray, integer(0L)) == gmp::as.bigq(0L))
  expect_true(getCoefficient(qspray, 0L) == gmp::as.bigq(0L))
  expect_true(getCoefficient(qspray, 1L) == gmp::as.bigq(0L))
  expect_true(getCoefficient(qspray, c(0L, 1L)) == gmp::as.bigq(1L))
  #
  qspray <- qlone(2) + 3L
  expect_true(getCoefficient(qspray, integer(0L)) == gmp::as.bigq(3L))
  expect_true(getCoefficient(qspray, 0L) == gmp::as.bigq(3L))
  #
  qspray <- qone()
  expect_true(getCoefficient(qspray, integer(0L)) == gmp::as.bigq(1L))
  expect_true(getCoefficient(qspray, 0L) == gmp::as.bigq(1L))
  #
  qspray <- qzero()
  expect_true(getCoefficient(qspray, integer(0L)) == gmp::as.bigq(0L))
  expect_true(getCoefficient(qspray, 0L) == gmp::as.bigq(0L))
})