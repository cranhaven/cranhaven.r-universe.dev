testthat::test_that("StrategyConfiguration: minNumClusters function works", {

  configuration <- StrategyConfiguration$new()

  testthat::expect_equal(configuration$minNumClusters(), 2)
  testthat::expect_message(configuration$minNumClusters(),
                           "[StrategyConfiguration][INFO] Using default minCluster configuration: 2 clusters minimun",
                           fixed = TRUE)
})

testthat::test_that("StrategyConfiguration: maxNumClusters function works", {

  configuration <- StrategyConfiguration$new()

  testthat::expect_equal(configuration$maxNumClusters(), 50)
  testthat::expect_message(configuration$maxNumClusters(),
                           "[StrategyConfiguration][INFO] Using default maxCluster configuration: 50 clusters maximun",
                           fixed = TRUE)
})
