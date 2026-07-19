test_that("the function throws no errors with one new sample and a one component model", {
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

  # Remove a sample and define
  i = 1
  Xtest = lapply(Z$object, function(x){x@data[i,,]})
  Ytest = Y[i]

  Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,1,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_no_error(npred(model, Xtest, Ztrain))
})

test_that("the function throws no errors with one new sample and a two component model", {
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

  # Remove a sample and define
  i = 1
  Xtest = lapply(Z$object, function(x){x@data[i,,]})
  Ytest = Y[i]

  Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,2,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_no_error(npred(model, Xtest, Ztrain))
})

test_that("the function throws no errors with several new samples", {
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

  # Remove a sample and define
  i = c(1,2)
  Xtest = lapply(Z$object, function(x){x@data[i,,]})
  Ytest = Y[i]

  Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,2,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_no_error(npred(model, Xtest, Ztrain))
})

test_that("the function throws no errors for the tensor-matrix case", {
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

  df1 = reinflateTensor(A, B, C)
  df2 = reinflateMatrix(A, D)
  datasets = list(df1, df2)
  modes = list(c(1,2,3), c(1,4))
  Z = setupCMTFdata(datasets, modes)
  Y = matrix(A[,1])

  # Remove a sample and define
  i = c(1,2)
  Xtest = list()
  for(p in 1:length(Z$object)){
    if(length(dim(Z$object[[p]]))==3){
      Xtest[[p]] = Z$object[[p]]@data[i,,]
    } else{
      Xtest[[p]] = Z$object[[p]]@data[i,]
    }
  }
  Ytest = Y[i]

  Xtrain = list()
  for(p in 1:length(Z$object)){
    if(length(dim(Z$object[[p]]))==3){
      Xtrain[[p]] = Z$object[[p]]@data[-i,,]
    } else{
      Xtrain[[p]] = Z$object[[p]]@data[-i,]
    }
  }
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,2,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_no_error(npred(model, Xtest, Ztrain))
})

test_that("vectX must be the same size as vectZ", {
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

  # Remove a sample and define
  i = 1
  Xtest = lapply(Z$object, function(x){x@data[i,1:3,]})
  Ytest = Y[i]

  Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,2,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_error(npred(model, Xtest, Ztrain))
})

test_that("missing values in Xnew are ignored for the prediction", {
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

  # Remove a sample and define
  i = 1
  Xtest = lapply(Z$object, function(x){x@data[i,,]})
  Xtest[[1]][1:3,] = NA
  Ytest = Y[i]

  Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
  Ytrain = Y[-i]
  Ztrain = setupCMTFdata(Xtrain, Z$modes)

  model = acmtfr_opt(Ztrain,Ytrain,2,initialization="random",pi=0, nstart=1, max_iter=10)
  expect_no_error(npred(model, Xtest, Ztrain))
})

test_that("yhat and npred of the input data are equal for ACMTF", {
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

  model = acmtfr_opt(Z,Y,1,initialization="random",pi=1, nstart=1)
  Yhat = model$Yhat
  Ypred = as.matrix(npred(model, lapply(Z$object, FUN=function(x){x@data}), Z))

  expect_equal(Yhat, Ypred, tolerance=0.05)
})

test_that("yhat and npred of the input data are equal for ACMTF-R", {
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
  Z = setupCMTFdata(datasets, modes, normalize=FALSE)
  Y = matrix(A[,1])

  model = acmtfr_opt(Z,Y,1,initialization="random",pi=0.5, nstart=1)
  Yhat = model$Yhat
  Ypred = as.matrix(npred(model, lapply(Z$object, FUN=function(x){x@data}), Z))

  expect_equal(Yhat, Ypred, tolerance=0.05)
})
