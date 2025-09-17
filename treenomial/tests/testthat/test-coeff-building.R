context("Building coefficient matrices")
library(treenomial)
library(ape)

## tests on treeToPoly ##
test_that("Test consistency of treeToPoly runs tips = 120", {
  numTips <- 120
  tree <- rtree(n = numTips)
  expect_equal(treeToPoly(tree, numThreads = 0), treeToPoly(tree, numThreads = 0))
})

## tests on allTrees ##
test_that("ensure that number of trees from allTrees match Wedderburn-Etherington numbers up to 13 tips", {
  wadNum <- c(1,1,1,2,3,6, 11, 23, 46, 98, 207, 451,983)

  # 2179, 4850, 10905)

  # check real version
  allTrees <- allTrees(13)
  allTrees <- unique(allTrees)
  expect_equal(wadNum,lengths(allTrees))

  # check complex version
  allTrees <- allTrees(13, type = "yEvaluated", y = 1+1i)
  allTrees <- unique(allTrees)
  expect_equal(wadNum,lengths(allTrees))

  allTrees <- allTrees(13, type = "phylo")
  allTrees <- unique(allTrees)
  expect_equal(wadNum,lengths(allTrees))
})

## tests on alignment ##
test_that("test different size trees to real polynomials", {
  differentSizeTrees <- c(rtree(2), rmtree(10,10))

  coeffs <- treeToPoly(differentSizeTrees, numThreads = 0)
  coeffs <- alignPoly(coeffs)

  firstDims <- dim(coeffs[[1]])

  res <- lapply(coeffs, function(i)  dim(i) == firstDims)
  expect_condition(all(res))
})

test_that("test different size trees to complex polynomials", {
  differentSizeTrees <- c(rtree(2), rmtree(10,10))

  coeffs <- treeToPoly(differentSizeTrees, type = "yEvaluated", y = 1+1i, numThreads = 0)
  coeffs <- alignPoly(coeffs)

  firstDims <- dim(coeffs[[1]])

  res <- lapply(coeffs, function(i)  dim(i) == firstDims)
  expect_condition(all(res))
})


test_that("test different size trees with binary trait labels", {

  largerTree <- rtree(30)
  largerTree$tip.label <- sample(c("t1","t2"),size = 30 , replace = TRUE)

  smallerTree <- rtree(2)

  coeffs <- treeToPoly(c(largerTree,smallerTree), type = "tipLabel", numThreads = 0)

  coeffs <- alignPoly(coeffs)

  firstDims <- dim(coeffs[[1]])

  res <- lapply(coeffs, function(i)  dim(i) == firstDims)
  expect_condition(all(res))


  largerTrees <- rmtree(10,10)

  largerTrees <- lapply(largerTrees, function(i){
    i$tip.label <- sample(c("t1","t2"),size = 10 , replace = TRUE)
    return(i)
  })

  differentSizeTrees <- c(largerTrees, list(rtree(2)))

  coeffs <- treeToPoly(differentSizeTrees, type = "tipLabel", numThreads = 0)

  coeffs <- alignPoly(coeffs)

  firstDims <- dim(coeffs[[1]])

  res <- lapply(coeffs, function(i)  dim(i) == firstDims)
  expect_condition(all(res))


  largerTrees <- rmtree(10,10)

  largerTrees <- lapply(largerTrees, function(i){
    i$tip.label <- sample(c("t1","t2"),size = 10 , replace = TRUE)
    return(i)
  })

  smallerTrees <- rmtree(50,5)

  smallerTrees <- lapply(smallerTrees, function(i){
    i$tip.label <- sample(c("t1","t2"),size = 5, replace = TRUE)
    return(i)
  })

  differentSizeTrees <- c(largerTrees, smallerTrees)

  coeffs <- treeToPoly(differentSizeTrees, type = "tipLabel", numThreads = 0)

  coeffs <- alignPoly(coeffs)

  firstDims <- dim(coeffs[[1]])

  res <- lapply(coeffs, function(i)  dim(i) == firstDims)
  expect_condition(all(res))

})

