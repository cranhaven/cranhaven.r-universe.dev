test_that("acmtf_fg works under normal circumstances", {
  set.seed(123)

  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")

  expect_no_error(acmtf_fg(fac_to_vect(result), Z))
})

test_that("f is the same as acmtf_fun", {
  set.seed(123)

  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  f = acmtf_fun(fac_to_vect(result), Z)
  fg_result = acmtf_fg(fac_to_vect(result),Z)
  expect_equal(fg_result$fn, f)
})

test_that("g is the same as acmtf_gradient", {
  set.seed(123)

  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  g = acmtf_gradient(fac_to_vect(result), Z)
  fg_result = acmtf_fg(fac_to_vect(result),Z)
  expect_equal(fg_result$gr, g)
})
