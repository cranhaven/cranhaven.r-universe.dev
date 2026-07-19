test_that("f is greater than zero when any solution is found", {
  I = 108
  J = 100
  K = 10

  df = array(rnorm(I*J*K), c(I,J,K))
  Y = rnorm(I)
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  f = acmtfr_fun(fac_to_vect(result), Z, Y, pi=0.5)
  expect_gt(f, 0)
})

test_that("an error is thrown for 4-way or more", {
  I = 108
  J = 100
  K = 10
  L = 5
  df = array(rnorm(I*J*K*L), c(I,J,K,L))
  datasets = list(df, df)
  modes = list(c(1,2,3,4), c(1,5,6,7))
  Z = setupCMTFdata(datasets, modes)
  result = initializeCMTF(Z, 1, initialization="random")

  expect_error(acmtfr_fun(fac_to_vect(result), Z))
})

test_that("f is different between different pi values", {
  I = 108
  J = 100
  K = 10

  df = array(rnorm(I*J*K), c(I,J,K))
  Y = rnorm(I)
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  f1 = acmtfr_fun(fac_to_vect(result), Z, Y, pi=0.1)
  f2 = acmtfr_fun(fac_to_vect(result), Z, Y, pi=0.9)
  expect_true(f1 != f2)
})
