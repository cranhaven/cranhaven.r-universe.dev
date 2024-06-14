context("Individual feature selection search algorithm interface")
algorithms <- list(selectKBest, selectPercentile, selectThreshold, selectThresholdRange, selectDifference, selectSlope)
for (algorithm in algorithms) {
  result <- algorithm()(iris, 'Species', giniIndex())
  test_that(paste(attr(algorithm(),'name')," output required fields"), {
    expect_true(!is.null(result$bestFeatures))
    expect_true(!is.null(result$featuresSelected))
    expect_true(!is.null(result$valuePerFeature))
  })
  test_that(paste(attr(algorithm(),'name')," values are correct"), {
    expect_true(all(is.element(result$bestFeatures, c(0,1))))
    expect_true(all(result$bestFeatures <= 1))
    expect_true(all(result$bestFeatures >= 0))
  })
  test_that(paste(attr(algorithm(),'name')," columns number are correct"), {
    expect_equal(ncol(result$bestFeatures), ncol(iris) - 1)
  })
  test_that(paste(attr(algorithm(),'name')," features are dataset features"), {
    expect_true(all(result$featuresSelected %in% colnames(iris)))
  })
  test_that(paste(attr(algorithm(),'name')," features have a value"), {
    expect_equal(ncol(result$featuresSelected), ncol(result$valuePerFeature))
  })
}
