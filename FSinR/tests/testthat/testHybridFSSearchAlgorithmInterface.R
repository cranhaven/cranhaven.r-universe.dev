context("Hybrid feature selection search algorithm interface")
algorithms <- list(LCC)
for (algorithm in algorithms) {
  result <- algorithm()(iris, 'Species', cramer(), giniIndex())
  test_that(paste(attr(algorithm(),'name')," output required fields"), {
    expect_true(!is.null(result$bestFeatures))
    expect_true(!is.null(result$bestFitness))
  })
  test_that(paste(attr(algorithm(),'name')," values are correct"), {
    expect_true(all(is.element(result$bestFeatures, c(0,1))))
    expect_true(all(result$bestFeatures <= 1))
    expect_true(all(result$bestFeatures >= 0))
  })
  test_that(paste(attr(algorithm(),'name')," columns number are correct"), {
    expect_equal(ncol(result$bestFeatures), ncol(iris) - 1)
  })
  test_that(paste(attr(algorithm(),'name')," fitness value is in correct range"), {
    expect_true(all(result$bestFitness <= 1))
    expect_true(all(result$bestFitness >= 0))
  })
  test_that(paste(attr(algorithm(),'name')," stops if an individual measure is used as a set measure"), {
    expect_error(algorithm()(iris, 'Species', cramer(), relief()), 'Only feature set measures can be used')
  })
  
}
