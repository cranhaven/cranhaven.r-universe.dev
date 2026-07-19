test_that("setupCMTFdata works with N blocks", {
  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df, df, df, df)
  modes = list(c(1,2,3), c(1,4,5), c(1,6,7), c(1,8,9), c(1,10,11))
  expect_no_error(setupCMTFdata(datasets, modes))
})

test_that("setupCMTFdata throws errors if you don't supply modes", {
  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  expect_error(setupCMTFdata(datasets))
})

test_that("setupCMTFdata identifies the sizes of the datasets correctly", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  expect_equal(Z$sizes, c(108,100,10,100,10))
})

test_that("setupCMTFdata creates a correct missing tensor", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))

  dfMissing = df2
  dfMissing[1,1,1] = NA
  datasets = list(df1, dfMissing)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)

  expect_equal(Z$missing[[2]]@data[1,1,1], 0)
})

test_that("setupCMTFdata sets missing values to zero", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))

  dfMissing = df2
  dfMissing[1,1,1] = NA
  datasets = list(df1, dfMissing)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  expect_equal(Z$object[[2]]@data[1,1,1], 0)
})

test_that("setupCMTFdata normalizes correctly when requested", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=TRUE)

  expect_equal(rTensor::fnorm(Z$object[[1]]), 1)
})

test_that("setupCMTFdata does not normalize when requested", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  expect_equal(rTensor::fnorm(Z$object[[1]]), Z$norms[1])
})

test_that("setupCMTFdata when not normalizing does not change the original data", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))
  E = array(rnorm(10*2), c(10, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  expect_equal(df1[1,1,1], Z$object[[1]]@data[1,1,1])
})

test_that("setupCMTFdata works with a tensor-matrix combination", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100, 2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)

  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  expect_no_error(setupCMTFdata(datasets, modes, normalize=FALSE))
})
