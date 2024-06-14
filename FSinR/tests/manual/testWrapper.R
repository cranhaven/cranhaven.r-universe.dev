context("Search + Wrapper")

data1 <- get(load('../data/dataClass.RData'))
data2 <- get(load("../data/dataReg.RData"))

test_that("Classification", {

  # Wrapper method
  resamplingParams <- list(method = "cv", number = 5)
  fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = seq(1,10,by=2)))
  wra <- wrapper("knn",resamplingParams, fittingParams) # wrapper method

  # SFS
  res <- sequentialForwardSelection()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SFFS
  res <- sequentialFloatingForwardSelection()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SBS
  res <- sequentialBackwardSelection()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SFBS
  res <- sequentialFloatingBackwardSelection()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # BFS
  res <- breadthFirst()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # DFS
  res <- deepFirst()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # GA
  res <- geneticAlgorithm(maxiter=15)(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # WOA

  # ACO
  res <- antColony(iter=15)(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SA
  res <- simulatedAnnealing()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # HC

  # TS
  res <- tabu(iter=50, tamTabuList=3, intensification=1, diversification=1)(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # LasVegas
  res <- LasVegas()(data1, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

})

test_that("Regression", {

  # Wrapper method
  resamplingParams <- list(method = "cv", number = 3)
  fittingParams <- list(preProcess = c("center", "scale"), metric="RMSE")
  wra <- wrapper("lm",resamplingParams, fittingParams) # wrapper method

  # SFS
  res <- sequentialForwardSelection()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SFFS
  res <- sequentialFloatingForwardSelection()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SBS
  res <- sequentialBackwardSelection()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SFBS
  res <- sequentialFloatingBackwardSelection()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # BFS
  res <- breadthFirst()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # DFS
  res <- deepFirst()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # GA
  res <- geneticAlgorithm(maxiter=15)(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # WOA

  # ACO
  res <- antColony(iter=15)(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # SA
  res <- simulatedAnnealing()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # HC

  # TS
  res <- tabu(iter=50, tamTabuList=3, intensification=1, diversification=1)(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

  # LasVegas
  res <- LasVegas()(data2, 'y', wra)[[1]]
  features <- colnames(res)[which(res==1)]
  features <- paste(features,collapse=" ")
  expect_match( features , 'x1' )

})
