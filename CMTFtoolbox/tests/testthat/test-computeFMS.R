test_that("Normal input does not give errors", {
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  Fac1 = list(A,B,C,D,E)
  Fac2 = Fac1
  modes = list(c(1,2,3), c(1,4,5))

  expect_no_error(computeFMS(Fac1, Fac2, modes))
})

test_that("FMS outcome is length equal to the number of datasets", {
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))
  F = array(rnorm(100*2), c(100,2))
  G = array(rnorm(10*2), c(10,2))

  Fac1 = list(A,B,C,D,E,F,G)
  Fac2 = Fac1
  modes = list(c(1,2,3), c(1,4,5), c(1,6,7))

  expect_equal(length(computeFMS(Fac1, Fac2, modes)), 3)
})

test_that("identical models have an FMS of one", {
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  Fac1 = list(A,B,C,D,E)
  Fac2 = Fac1
  modes = list(c(1,2,3), c(1,4,5))

  expect_equal(computeFMS(Fac1, Fac2, modes), c(1,1))
})

test_that("non-identical models have an FMS of less than one", {
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  Fac1 = list(A,B,C,D,E)

  G = array(rnorm(108*2), c(108, 2))
  H = array(rnorm(100*2), c(100, 2))
  I = array(rnorm(10*2), c(10, 2))
  J = array(rnorm(100*2), c(100, 2))
  K = array(rnorm(10*2), c(10, 2))

  Fac2 = list(G,H,I,J,K)
  modes = list(c(1,2,3), c(1,4,5))
  FMS_result = computeFMS(Fac1, Fac2, modes)
  expect_true(FMS_result[1] < 1 & FMS_result[2] < 1)
})

test_that("non-identical models have an FMS of greater than or equal to zero", {
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  Fac1 = list(A,B,C,D,E)

  G = array(rnorm(108*2), c(108, 2))
  H = array(rnorm(100*2), c(100, 2))
  I = array(rnorm(10*2), c(10, 2))
  J = array(rnorm(100*2), c(100, 2))
  K = array(rnorm(10*2), c(10, 2))

  Fac2 = list(G,H,I,J,K)
  modes = list(c(1,2,3), c(1,4,5))
  FMS_result = computeFMS(Fac1, Fac2, modes)
  expect_true(FMS_result[1] >= 0 & FMS_result[2] >= 0)
})
