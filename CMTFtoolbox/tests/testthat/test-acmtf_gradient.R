test_that("g is produced if any solution is found", {
  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  expect_no_error(acmtf_gradient(fac_to_vect(result), Z))
})

test_that("the size of g is correct in the two-tensor case", {
  I = 108
  J = 100
  K = 10
  df = array(rnorm(I*J*K), c(I,J,K))
  datasets = list(df, df)
  modes = list(c(1,2,3), c(1,4,5))
  Z = setupCMTFdata(datasets, modes)
  result = initializeACMTF(Z, 1, initialization="random")
  g = acmtf_gradient(fac_to_vect(result), Z)
  expect_equal(length(g), I+J+K+J+K+2)
})

test_that("the size of g is correct in the tensor-matrix case", {
  A = array(rnorm(108))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10))

  df1 = reinflateTensor(A, B[,1], C)
  df2 = reinflateMatrix(A, B[,2])
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)
  result = initializeACMTF(Z, 1, initialization="random")

  g = acmtf_gradient(fac_to_vect(result), Z)
  expect_equal(length(g), 108+100+10+100+2)
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

  expect_error(acmtf_gradient(fac_to_vect(result), Z))
})
