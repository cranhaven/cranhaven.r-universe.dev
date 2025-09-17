context("Results from the wedge function")
library(treenomial)


test_that("Check wedging result real poly", {
  cherry <- matrix(c(0, 1, 0, 0, 1, 0), nrow = 2) # x^2 + y
  cherryWedged <- matrix(c(0,1,1,0,0,0,0,2,0,0,0,0,1,0,0), nrow = 3) # x^4 + 2x^2y + y^2 + y
  expect_equal(wedge(cherry, cherry), cherryWedged)

  eightTipTree <- treeToPoly(ape::stree(8,type = "balanced"), numThreads = 0)
  expect_equal(wedge(cherryWedged, cherryWedged), eightTipTree)

})

test_that("Check wedging result complex poly", {

  leaf <- as.complex(c(0,1))
  cherry <- wedge(leaf, leaf, type = "yEvaluated", y =(1+1i))

  cherryWedged <- c(1+3i,0,2i+2,0,1)

  expect_equal(wedge(cherry, cherry, type = "yEvaluated", y = (1+1i)), cherryWedged)
})

test_that("Check wedging result phylo type", {

  res <- allTrees(2, type = "phylo")[[2]]

  expect_equal(wedge("leaf", "leaf", type = "phylo"), res[[1]])
})



