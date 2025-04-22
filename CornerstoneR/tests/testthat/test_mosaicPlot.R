context("Mosaic Plot")

test_that("Mosaic Plot", {
  set.seed(1619)
  env = globalenv()
  
  # create CS-R output
  createCSEnvir(titanic
                , strPreds = c("Class", "Age", "Sex", "Survived")
                , env = env
                )
  # init cs.* functions (only necessary for local call)
  createCSFunctions(env = env)
  # run mosaic plot
  expect_true(mosaicPlot())
  res = mosaicPlot(return.results = TRUE)
  expect_list(res, len = 1)
  expect_data_frame(res$long.contingency)
  expect_equal(sum(res$long.contingency$Freq), 2201)
  
  # multi classes
  # create CS-R output
  createCSEnvir(titanic
                , strPreds = c("Age", "Sex", "Survived"), strResps = "Class"
                , env = env
                )
  # run mosaic plot
  expect_true(mosaicPlot())
  
  # multi responses
  # create CS-R output
  createCSEnvir(titanic
                , strPreds = c("Class", "Age"), strResps = c("Survived", "Sex")
                , env = env
                )
  # run mosaic plot
  expect_true(mosaicPlot())
  
  # numeric response
  titanic[, Survived := as.numeric(Survived)]
  # create CS-R output
  createCSEnvir(titanic
                , strPreds = c("Class", "Age", "Sex"), strResps = c("Survived")
                , env = env
                )
  # run mosaic plot
  expect_true(mosaicPlot())
  # reload data
  data("titanic")
  
})
