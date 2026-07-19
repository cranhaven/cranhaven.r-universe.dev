test_that("fac_to_vect and vect_to_fac work correctly in the CMTF case", {
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

  init = initializeCMTF(Z, 2)

  vect = fac_to_vect(init)
  Fac = vect_to_fac(fac_to_vect(init), Z, sortComponents=FALSE)
  expect_equal(Fac, init)
})

test_that("fac_to_vect and vect_to_fac work correctly in the ACMTF case", {
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

  init = initializeACMTF(Z, 2)

  vect = fac_to_vect(init)
  Fac = vect_to_fac(fac_to_vect(init), Z, sortComponents=FALSE)
  expect_equal(Fac, init)
})

test_that("vect_to_fac resorts components", {
  set.seed(456)
  A = array(rnorm(108*5), c(108, 5))
  B = array(rnorm(100*5), c(100, 5))
  C = array(rnorm(10*5), c(10, 5))
  D = array(rnorm(100*5), c(100,5))

  # Inject an ordering
  componentSizes = c(3, 5, 9, 8, 1)
  for(i in 1:5){
    A[,i] = A[,i] * componentSizes[i]
    B[,i] = B[,i] * componentSizes[i]
    C[,i] = C[,i] * componentSizes[i]
    D[,i] = D[,i] * componentSizes[i]
  }

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result1 = cmtf_opt(Z, 5, initialization="nvec", max_iter=5, sortComponents=FALSE)
  result2 = cmtf_opt(Z, 5, initialization="nvec", max_iter=5, sortComponents=TRUE)
  expect_false(all(unlist(result1$Fac) == unlist(result2$Fac)))
})

test_that("vect_to_fac has correct loadings for a dataset that is a vector with sortComponents FALSE", {
  numComponents = 3
  I = 108
  J = 100
  K = 10
  L = 1
  A = array(rnorm(I*numComponents), c(I, numComponents))  # shared subject mode
  B = array(rnorm(J*numComponents), c(J, numComponents))  # distinct feature mode of X1
  C = array(rnorm(K*numComponents), c(K, numComponents))  # distinct condition mode of X1
  D = t(as.matrix(c(1,1,1)))
  lambdas = matrix(c(1,1,1,1,1,1),nrow=2,ncol=numComponents)

  df1 = array(0L, c(I, J, K))
  df2 = array(0L, c(I, L))
  for(i in 1:numComponents){
    df1 = df1 + lambdas[1,i] * reinflateTensor(A[,i], B[,i], C[,i])
    df2 = df2 + lambdas[2,i] * reinflateMatrix(A[,i], D[,i])
  }
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  vec = initializeACMTF(Z, numComponents, output="vect")
  Fac = vect_to_fac(vec, Z, sortComponents=FALSE)
  expect_equal(dim(Fac[[4]]), c(1,numComponents))
})

test_that("vect_to_fac has correct loadings for a dataset that is a vector with sortComponents TRUE", {
  numComponents = 3
  I = 108
  J = 100
  K = 10
  L = 1
  A = array(rnorm(I*numComponents), c(I, numComponents))  # shared subject mode
  B = array(rnorm(J*numComponents), c(J, numComponents))  # distinct feature mode of X1
  C = array(rnorm(K*numComponents), c(K, numComponents))  # distinct condition mode of X1
  D = t(as.matrix(c(1,1,1)))
  lambdas = matrix(c(1,1,1,1,1,1),nrow=2,ncol=numComponents)

  df1 = array(0L, c(I, J, K))
  df2 = array(0L, c(I, L))
  for(i in 1:numComponents){
    df1 = df1 + lambdas[1,i] * reinflateTensor(A[,i], B[,i], C[,i])
    df2 = df2 + lambdas[2,i] * reinflateMatrix(A[,i], D[,i])
  }
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)

  vec = initializeACMTF(Z, numComponents, output="vect")
  Fac = vect_to_fac(vec, Z, sortComponents=TRUE)
  expect_equal(dim(Fac[[4]]), c(1,numComponents))
})

test_that("removeTwoNormCol indeed removed the two-norm", {
  df = array(rnorm(108,2), c(108,2))
  result = removeTwoNormCol(df)
  expect_equal(norm(result[,1],"2"), 1)
})

test_that("normalizeFac throws no errors", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  expect_no_error(normalizeFac(Fac, modes))
})

test_that("normalizeFac indeed normalized the factors", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_equal(apply(output$Fac[[1]], 2, function(x){norm(as.matrix(x),"F")}), c(1,1))
})

test_that("normalizeFac$Fac output dimensions are the same as the input", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_equal(lapply(Fac, dim), lapply(output$Fac, dim))
})

test_that("normalizeFac$normsPerDataset output dimensions are correct", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_equal(dim(output$normsPerDataset), c(2,2))
})

test_that("normalizeFac$normsPerDataset norms are minimum 1", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_true(all(output$normsPerDataset >= 1))
})

test_that("normalizeFac$normsPerLoading output dimensions are correct", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_equal(dim(output$normsPerLoading), c(4,2))
})

test_that("normalizeFac$normsPerLoading norms are minimum 1", {
  A = array(rnorm(108*2), c(108,2))
  B = array(rnorm(100*2), c(100,2))
  C = array(rnorm(10*2), c(10,2))
  D = array(rnorm(100*2), c(100,2))
  Fac = list(A,B,C,D)
  modes = list(c(1,2,3), c(1,4))
  output = normalizeFac(Fac, modes)
  expect_true(all(output$normsPerLoading >= 1))
})

test_that("calculateVarExp does not throw errors", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  inputFac = list(A,B,C,D)

  expect_no_error(calculateVarExp(result$Fac, Z))
})

test_that("calculateVarExp has a lower bound of zero", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  inputFac = list(A,B,C,D)

  varExps = calculateVarExp(result$Fac, Z)

  expect_true(all(varExps >= 0))
})

test_that("calculateVarExp has an upper bound of one", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  inputFac = list(A,B,C,D)

  varExps = calculateVarExp(result$Fac, Z)

  expect_true(all(varExps <= 1))
})

test_that("calcVarExpPerComponent does not throw errors", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  expect_no_error(calcVarExpPerComponent(result$Fac, Z))
})

test_that("calcVarExpPerComponent values are minimum zero", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  varExps = calcVarExpPerComponent(result$Fac, Z)
  expect_true(all(varExps >= 0))
})

test_that("calcVarExpPerComponent values are maximum one", {
  set.seed(123)
  A = array(rnorm(108*2), c(108, 2))
  B = array(rnorm(100*2), c(100, 2))
  C = array(rnorm(10*2), c(10, 2))
  D = array(rnorm(100*2), c(100,2))

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)

  result = cmtf_opt(Z, 2, initialization="nvec", max_iter=5)
  varExps = calcVarExpPerComponent(result$Fac, Z)
  expect_true(all(varExps <= 1))
})

test_that("findSharedModes does not throw errors", {
  l = list(c(1,2,3), c(1,4,5))
  expect_no_error(findSharedModes(l))
})

test_that("findSharedModes finds multiple modes if applicable", {
  l = list(c(1,2,3), c(1,4,3), c(1,5,3))
  expect_equal(findSharedModes(l), c(1,3))
})

test_that("findSharedModes throws an error if no shared modes are found", {
  l = list(c(1,2,3), c(4,5,6), c(7,8,9))
  expect_error(findSharedModes(l))
})

test_that("safePseudoInverse handles non-invertible matrix gracefully", {
# Create a rank-deficient matrix
 M <- matrix(c(1:5, 1:5), nrow = 5, ncol = 2)
 # We'll do t(M) %*% M => a singular 2x2 matrix
 M_singular <- t(M) %*% M  # definitely rank 1 if columns are multiples
 expect_no_error(safePseudoInverse(M_singular, mu = 1e-6))
})

test_that("safeSolve handles non-invertible matrix gracefully", {

 M <- matrix(c(1:4, 1:4), nrow=4, byrow = TRUE)
 M_singular <- t(M) %*% M
 # Attempt safeSolve
 expect_no_error(safeSolve(M_singular, mu = 1e-6))
})

