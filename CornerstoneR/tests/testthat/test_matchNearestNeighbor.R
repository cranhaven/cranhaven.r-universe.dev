context("Match Nearest Neighbor")

test_that("Match Nearest Neighbor", {
  set.seed(1619)
  env = globalenv()
  
  # create CS-R output
  createCSEnvir(iris
                , strPreds = names(iris)[1:3], strAuxs = names(iris)[4:5]
                , lstScriptVars = list(remove.pattern = "")
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # get redirected Dataset
  redirectedDataset = redirectDataset(return.results = TRUE)
  
  # create CS-R output
  iris.small = iris[sample(150, 50), ]
  iris.small[, 1:4] = iris.small[, 1:4] + rnorm(4*150, sd = 0.01)
  createCSEnvir(iris.small[, c(1:3, 5)]
                , strPreds = names(iris.small)[1:3], strAuxs = names(iris.small)[5]
                , lstRobject = redirectedDataset
                , env = env
                )
  # run match nearest neighbor
  expect_true(matchNearestNeighbor())
  res = matchNearestNeighbor(return.results = TRUE)
  expect_list(res, len = 2)
  expect_data_table(res$nearest.neighbors, nrows = 50, ncols = 10)
  expect_equal(res$nearest.neighbors$Species, res$nearest.neighbors$Species.redir)
  expect_data_table(res$runtimes, nrows = 1, ncols = 2)
  
  # create CS-R output with multiple robjects
  iris.small = iris[sample(150, 50), ]
  iris.small[, 1:4] = iris.small[, 1:4] + rnorm(4*150, sd = 0.01)
  createCSEnvir(iris.small[, c(1:3, 5)]
                , strPreds = names(iris.small)[1:3], strAuxs = names(iris.small)[5]
                , lstRobject = rep(redirectedDataset, 3)
                , env = env
                )
  # run match nearest neighbor
  expect_true(matchNearestNeighbor())
  res = matchNearestNeighbor(return.results = TRUE)
  expect_list(res, len = 2)
  expect_data_table(res$nearest.neighbors, nrows = 50, ncols = 22)
  expect_equal(res$nearest.neighbors$Species, res$nearest.neighbors$Species.red.3)
  expect_data_table(res$runtimes, nrows = 3, ncols = 2)
  
  # remove redirected class
  redirectedDataset2 = redirectedDataset
  class(redirectedDataset2$robjects) = "list"
  # create CS-R output
  iris.small = iris[sample(150, 50), ]
  iris.small[, 1:4] = iris.small[, 1:4] + rnorm(4*150, sd = 0.01)
  createCSEnvir(iris.small[, c(1:3, 5)]
                , strPreds = names(iris.small)[1:3], strAuxs = names(iris.small)[5]
                , lstRobject = redirectedDataset2
                , env = env
                )
  # run match nearest neighbor
  expect_true(matchNearestNeighbor())
  res = matchNearestNeighbor(return.results = TRUE)
  expect_data_table(res$nearest.neighbors, nrows = 50, ncols = 4)
})
