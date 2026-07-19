test_that("a solution is found in the two-tensor case and Y", {
  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  Y = matrix(rnorm(I), nrow=I, ncol=1)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  expect_no_error(acmtfr_opt(Z, Y, 1, max_iter=2))
})

test_that("a solution is found when running LBFGS", {
  I = 10
  J = 5
  K = 3
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  Y = matrix(rnorm(I), nrow=I, ncol=1)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  expect_no_error(acmtfr_opt(Z, Y, 1, max_iter=2, method="L-BFGS"))
})

test_that("the objective is very high if an incorrect solution is found", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  result = acmtfr_opt(Z, Y, 2, initialization="random", max_iter = 2)
  expect_gt(result$f, 0)
})

test_that("allOutput=TRUE gives a list of expected length", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  results = acmtfr_opt(Z, Y, 2, initialization="random", nstart=2, max_iter=2, allOutput=TRUE)
  expect_equal(length(results), 2)
})

test_that("the sum of all loss terms is equal to f", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  model = acmtfr_opt(Z, Y, 2, initialization="random", nstart=1, max_iter=2)
  f = sum(model$f_per_block) + model$f_y + sum(model$f_norms) + sum(model$f_lambda)
  expect_equal(model$f, f)
})

test_that("running in parallel works", {
  skip_on_cran()

  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  expect_no_error(acmtfr_opt(Z,Y,2,initialization="random", nstart=2, max_iter=2, numCores=2))
})

test_that("different settings of pi yield a different fit", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  model1 = acmtfr_opt(Z,Y,2,initialization="nvec",pi=0.1, nstart=1, max_iter=10)
  model2 = acmtfr_opt(Z,Y,2,initialization="nvec",pi=0.9, nstart=1, max_iter=10)
  expect_true(model1$f != model2$f)
})

test_that("pi=1 gives post-hoc regression coefficients", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))
  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(rnorm(I), nrow=I, ncol=1)

  model = acmtfr_opt(Z,Y,2,initialization="nvec",pi=1, nstart=1, max_iter=2)
  expect_equal(dim(model$rho), c(2,1))
})

test_that("pi=0 throws no errors", {
  set.seed(123)
  I = 10
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(A[,1])

  expect_no_error(acmtfr_opt(Z,Y,2,initialization="random",pi=0, nstart=1, max_iter=2))
})

test_that("computing too many components is handled gracefully in the solve step", {
  set.seed(123)
  I = 21
  J = 5
  K = 3
  L = 8
  M = 3
  A = array(rnorm(I*2), c(I, 2))
  B = array(rnorm(J*2), c(J, 2))
  C = array(rnorm(K*2), c(K, 2))
  D = array(rnorm(L*2), c(L, 2))
  E = array(rnorm(M*2), c(M, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(A[,1])

  expect_no_error(acmtfr_opt(Z,Y,10,initialization="random",pi=0.95, nstart=1, max_iter=2))
})
