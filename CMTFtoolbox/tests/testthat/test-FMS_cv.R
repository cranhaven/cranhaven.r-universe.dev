test_that("the function works normally with 1 component", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  model1 = acmtf_opt(Z, 1, initialization="random", abs_tol = 0.1, rel_tol=0.1)

  Fac1 = model1$Fac[1:3]
  Fac2 = Fac1
  expect_no_error(FMS_cv(Fac1, Fac2))
})

test_that("the function works normally with 2 components", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  model1 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)

  Fac1 = model1$Fac[1:3]
  Fac2 = Fac1
  expect_no_error(FMS_cv(Fac1, Fac2))
})

test_that("two equal models have an FMS of 1", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  model1 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)

  Fac1 = model1$Fac[1:3]
  Fac2 = Fac1
  expect_equal(FMS_cv(Fac1, Fac2), 1)
})

test_that("two different models have an FMS of larger than 0", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  model1 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)
  model2 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)

  Fac1 = model1$Fac[1:3]
  Fac2 = model2$Fac[1:3]
  expect_gt(FMS_cv(Fac1, Fac2), 0)
})

test_that("two different models have an FMS of less than 1", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  model1 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)
  model2 = acmtf_opt(Z, 2, initialization="random", abs_tol = 0.1, rel_tol=0.1)

  Fac1 = model1$Fac[1:3]
  Fac2 = model2$Fac[1:3]
  expect_lt(FMS_cv(Fac1, Fac2), 1)
})
