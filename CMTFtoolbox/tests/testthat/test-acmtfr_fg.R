test_that("acmtfr_fg throws no errors under normal circumstances", {
  set.seed(123)

  I = 108
  J = 100
  K = 10

  df = array(rnorm(I*J*K), c(I,J,K))
  Y = rnorm(I)
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  expect_no_error(acmtfr_fg(fac_to_vect(result), Z, Y, pi=0.5))
})

test_that("f is the same as acmtfr_fun", {
  set.seed(123)

  I = 108
  J = 100
  K = 10

  df = array(rnorm(I*J*K), c(I,J,K))
  Y = rnorm(I)
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  f = acmtfr_fun(fac_to_vect(result),Z,Y,pi=0.5)
  fg_result = acmtfr_fg(fac_to_vect(result), Z, Y, pi=0.5)
  expect_equal(fg_result$fn, f)
})

test_that("g is the same as acmtfr_gradient", {
  set.seed(123)

  I = 108
  J = 100
  K = 10

  df = array(rnorm(I*J*K), c(I,J,K))
  Y = rnorm(I)
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  g = acmtfr_gradient(fac_to_vect(result),Z,Y,pi=0.5)
  fg_result = acmtfr_fg(fac_to_vect(result), Z, Y, pi=0.5)
  expect_equal(fg_result$gr, g)
})
