test_that("the function works normally", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  df2 = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Y = as.matrix(rnorm(I))

  expect_no_error(ACMTFR_modelSelection(datasets, modes, Y, maxNumComponents=2, pi=1.0, nstart=2, cvFolds=2, rel_tol=0.5, abs_tol=0.5))
})

test_that("the function works for the tensor-matrix case", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  df2 = array(rnorm(I*J), c(I,J))
  datasets = list(df, df2)
  modes = list(c(1,2,3), c(1,4))
  Y = as.matrix(rnorm(I))

  expect_no_error(ACMTFR_modelSelection(datasets, modes, Y, maxNumComponents=2, pi=1.0, nstart=2, cvFolds=2, rel_tol=0.5, abs_tol=0.5))
})

test_that("the function works when jack-knifing", {
  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  df2 = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Y = as.matrix(rnorm(I))

  expect_no_error(ACMTFR_modelSelection(datasets, modes, Y, maxNumComponents=2, pi=1.0, nstart=2, cvFolds=10, rel_tol=0.5, abs_tol=0.5))
})

test_that("running in parallel works", {
  skip_on_cran()

  set.seed(123)

  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  df2 = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Y = as.matrix(rnorm(I))

  expect_no_error(ACMTFR_modelSelection(datasets, modes, Y, maxNumComponents=2, pi=1.0, nstart=2, cvFolds=2, rel_tol=0.5, abs_tol=0.5, numCores=2))
})
