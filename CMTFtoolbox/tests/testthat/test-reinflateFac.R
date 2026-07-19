test_that("reinflateFac works for the CMTF case", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  result = reinflateFac(Fac, Z)
  expect_true(all.equal(result, datasets))
})

test_that("reinflateFac works for the ACMTF case", {
  numComponents = 3
  I = 108
  J = 100
  K = 10
  L = 100
  A = array(rnorm(I*numComponents), c(I, numComponents))  # shared subject mode
  B = array(rnorm(J*numComponents), c(J, numComponents))  # distinct feature mode of X1
  C = array(rnorm(K*numComponents), c(K, numComponents))  # distinct condition mode of X1
  D = array(rnorm(L*numComponents), c(L, numComponents))  # distinct feature mode of X2
  lambdas = array(c(0.5, 1, 0.25, 1, 0.45, 1), c(2,3))

  df1 = array(0L, c(I, J, K))
  df2 = array(0L, c(I, L))
  for(i in 1:numComponents){
    df1 = df1 + lambdas[1,i] * reinflateTensor(A[,i], B[,i], C[,i])
    df2 = df2 + lambdas[2,i] * reinflateMatrix(A[,i], D[,i])
  }
  datasets = list(df1, df2)
  Fac = list(A,B,C,D,lambdas)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  result = reinflateFac(Fac, Z)
  expect_true(all.equal(result, datasets))
})

test_that("reinflateFac works when including a vector Y", {
  numComponents = 1
  I = 108
  J = 100
  K = 10
  L = 1
  A = array(rnorm(I*numComponents), c(I, numComponents))  # shared subject mode
  B = array(rnorm(J*numComponents), c(J, numComponents))  # distinct feature mode of X1
  C = array(rnorm(K*numComponents), c(K, numComponents))  # distinct condition mode of X1
  D = as.matrix(1)
  lambdas = as.matrix(c(1, 1))

  df1 = array(0L, c(I, J, K))
  df2 = array(0L, c(I, L))
  for(i in 1:numComponents){
    df1 = df1 + lambdas[1,i] * reinflateTensor(A[,i], B[,i], C[,i])
    df2 = df2 + lambdas[2,i] * reinflateMatrix(A[,i], D[,i])
  }
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  Fac = list(A, B, C, as.matrix(1), lambdas)
  expect_no_error(reinflateFac(Fac, Z))
})
