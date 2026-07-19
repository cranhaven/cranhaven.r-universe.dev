test_that("the correct mode 1 size is returned", {
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

  result = initializeACMTF(Z, 1, initialization="random")
  expect_equal(nrow(result[[1]]), 108)
})

test_that("max(modes)+1 number of initialized components are returned", {
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

  result = initializeACMTF(Z, 1, initialization="random")
  expect_equal(length(result), 6)
})

test_that("the correct mode 1 components are found using nvecs", {
  set.seed(123)
  A = rnorm(108)
  B = rnorm(100)
  C = rnorm(10)
  D = rnorm(100)
  E = rnorm(10)

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  result = initializeACMTF(Z, 1, initialization="nvec")
  expect_equal(abs(cor(result[[1]], A))[1,1], 1, tolerance=0.01)
})

test_that("the correct mode 2 components are found using nvecs", {
  set.seed(123)
  A = rnorm(108)
  B = rnorm(100)
  C = rnorm(10)
  D = rnorm(100)
  E = rnorm(10)

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  result = initializeACMTF(Z, 1, initialization="nvec")
  expect_equal(abs(cor(result[[2]], B))[1,1], 1, tolerance=0.01)
})

test_that("the correct mode 3 components are found using nvecs", {
  set.seed(123)
  A = rnorm(108)
  B = rnorm(100)
  C = rnorm(10)
  D = rnorm(100)
  E = rnorm(10)

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateTensor(A, D, E)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  result = initializeACMTF(Z, 1, initialization="nvec")
  expect_equal(abs(cor(result[[3]], C))[1,1], 1, tolerance=0.01)
})

test_that("the correct number of lambda values are produced", {
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

  result = initializeACMTF(Z, 2, initialization="random")
  expect_equal(result[[6]], array(1, c(2,2)))
})

test_that("initialized values are norm 1", {
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

  result = initializeACMTF(Z, 2, initialization="random")
  expect_equal(norm(as.matrix(result[[1]][,1]), "F"), 1)
})
