context("Distances between coefficient matrices")
library(treenomial)
library(ape)


## tests on polyToDistMat ##
test_that("test that different distance method have correct result (small manual check)", {


  coeffs <- allTrees(6)[[6]]

  dLogDiff <- polyToDistMat(coeffs, method = "logDiff", numThreads = 0)

  dWLogDiff <- polyToDistMat(coeffs, method = "wLogDiff", numThreads = 0)

  testWlogL1 <- function(coeffA,coeffB){
    logDiffMat <- rowSums(log(1 + abs(coeffA-coeffB)))

    weightVect <- c(1,(1:(nrow(coeffA)-1)))^(-2)

    sum(logDiffMat*weightVect)

  }

  expect_equal(dWLogDiff[2,4], testWlogL1(coeffs[[2]], coeffs[[4]]))


  fakeCoeffA <- matrix(1,10,10)
  fakeCoeffB <- matrix(2,10,10)

  expect_equal(33.33,round(polyDist(fakeCoeffA,fakeCoeffB),2))

  fakeCoeffA <- matrix(0,10,10)
  fakeCoeffB <- matrix(0,10,10)

  expect_equal(0,round(polyDist(fakeCoeffA,fakeCoeffB),2))

  fakeCoeffA <- t(matrix(rep(1,100)))
  fakeCoeffB <- t(matrix(rep(2,100)))

  expect_equal(33.33,round(polyDist(fakeCoeffA,fakeCoeffB),2))


})

## tests on treeToDistMat ##
test_that("test distance matrix of same trees is zero for each method", {
  tree <- list(rtree(10))
  identicalForest <- rep(tree,10)

  distanceMatrix <- treeToDistMat(identicalForest, method = "logDiff", type = "yEvaluated", y=1+1i, numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "fraction", type = "yEvaluated", y=1+1i, numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "logDiff", numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "fraction", numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "wLogDiff", numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "pa", numThreads = 0)
  expect_true(all(distanceMatrix == 0))

  distanceMatrix <- treeToDistMat(identicalForest, method = "ap", numThreads = 0)
  expect_true(all(distanceMatrix == 0))


})

test_that("testing naming carry through", {
  trees <- c(rmtree(2,10),rmtree(2,131),rtree(2),rtree(3))
  names(trees) <- c(rep(c("smaller","larger"), each = 2),"twoTip","threeTip")
  d <- treeToDistMat(trees, numThreads = 0)
  expect_equal(rownames(d),names(trees))
  expect_equal(colnames(d),names(trees))
})


test_that("ensure distance matrix is symmetric", {

  distanceMatrix <- treeToDistMat(rmtree(100,20), numThreads = 0)

  expect_true(all(distanceMatrix == t(distanceMatrix)))
})


## tests on plotExtremeTrees ##
test_that("ensure correct min/max trees are being found", {
  forestTen <- rmtree(100,10)
  forestSixty <- rmtree(42,60)
  threeTip <- rtree(3)

  minTrees <- plotExtremeTrees(threeTip, c(forestSixty,threeTip,forestTen), 2, numThreads = 0)

  expect_equal(minTrees[[1]]$distance, 0)
  expect_equal(treeDist(minTrees[[1]]$tree,threeTip, numThreads = 0), 0)

  maxTrees <- plotExtremeTrees(threeTip, c(forestSixty,threeTip,forestTen), 2, comparison = "max", numThreads = 0)

  expect_equal(maxTrees[[1]]$distance, treeDist(threeTip,maxTrees[[1]]$tree, numThreads = 0))
})


## other tests ##
test_that("tests with single and lists arguments", {
  forestTen <- rmtree(100,10)
  forestSixty <- rmtree(42,60)
  threeTip <- rtree(3)

  expect_silent(treeDist(threeTip,threeTip, numThreads = 0))
  expect_silent(treeDist(threeTip,forestSixty, numThreads = 0))
  expect_error(treeDist(list(threeTip),forestSixty, numThreads = 0))
  expect_error(treeDist(list(threeTip),list(forestSixty), numThreads = 0))


  res <- treeDist(threeTip,forestSixty, numThreads = 0)
  expect_equal(length(res),42)

  res <- treeDist(threeTip,forestSixty, numThreads = 0, type = "yEvaluated", y = 1+1i)
  expect_equal(length(res),42)

  coeffsSixty <- treeToPoly(forestSixty, numThreads = 0)
  coeffsThree <- treeToPoly(threeTip, numThreads = 0)

  res <- polyDist(coeffsThree,coeffsSixty, numThreads = 0)
  expect_equal(length(res),42)

  coeffsSixty <- treeToPoly(forestSixty, numThreads = 0, type = "yEvaluated", y = 1+1i)
  coeffsThree <- treeToPoly(threeTip, numThreads = 0, type = "yEvaluated", y = 1+1i)

  res <- polyDist(coeffsThree,coeffsSixty, numThreads = 0)
  expect_equal(length(res),42)

  expect_silent(polyDist(coeffsThree,coeffsThree, numThreads = 0))
  expect_silent(polyDist(coeffsThree,coeffsSixty, numThreads = 0))
  expect_error(polyDist(list(coeffsThree),coeffsSixty, numThreads = 0))
  expect_error(polyDist(list(coeffsThree),list(coeffsSixty), numThreads = 0))

  expect_silent(plotExtremeTrees(threeTip, threeTip, 1, numThreads = 0))
  expect_silent(plotExtremeTrees(threeTip, forestSixty, 1, numThreads = 0))
  expect_error(plotExtremeTrees(list(threeTip),forestSixty,1, numThreads = 0))
  expect_error(plotExtremeTrees(list(threeTip),list(forestSixty),1, numThreads = 0))
})

