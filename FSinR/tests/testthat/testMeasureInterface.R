context("Evaluation Measure Interface")

resamplingParams <- list(method = "cv", number = 5)
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = seq(1, 10, by = 2)))
wra <- wrapperEvaluator("knn",resamplingParams, fittingParams)

measures <- list(MDLC(), chiSquared(), cramer(), binaryConsistency(), IEConsistency(), IEPConsistency(), roughsetConsistency(), mutualInformation(), gainRatio(), symmetricalUncertain(), fscore(), giniIndex(), Jd(), relief(), normalizedRelief(), normalizedReliefFeatureSetMeasure(), ReliefFeatureSetMeasure(), wra)
setMeasures <- list(MDLC(), binaryConsistency(), IEConsistency(), IEPConsistency(), roughsetConsistency(), mutualInformation(), gainRatio(), symmetricalUncertain(), giniIndex(), Jd(), ReliefFeatureSetMeasure(), wra)
individualMeasures <- list(chiSquared(), cramer(), fscore(), relief())
discreteMeasures <- list(binaryConsistency(), IEConsistency(), IEPConsistency(), roughsetConsistency(), mutualInformation(), gainRatio(), symmetricalUncertain())
mixedMeasures <- list(chiSquared(), cramer(), fscore(), giniIndex(), Jd(), relief(), normalizedRelief(), ReliefFeatureSetMeasure(), normalizedReliefFeatureSetMeasure(), wra)
continuousMeasures <- list(MDLC())

streetNumber <- rep(seq(1,12),10)
doorLetter <- rep(c("A","B","C"),40)
classValue <- rep(seq(1,3),40)
discreteDataset <- data.frame(streetNumber, doorLetter, classValue)

x1 <- seq(1,30)
x2 <- rep(seq(1,10),3)
classValue <- rep(c("A","B"),15)
testDataset <- data.frame(x1, x2, classValue)


for (measure in measures) {
  test_that(paste(attr(measure,'name')," has target attribute"), {
    expect_true((attr(measure,'target') == 'maximize') || (attr(measure,'target') == 'minimize'))
  })
}

for (measure in individualMeasures) {
  result <- measure(testDataset, 'classValue', c('x1'))
  test_that(paste(attr(measure,'name')," returns numeric value"), {
    expect_true(is.numeric(result))
  })
}


for (measure in setMeasures) {
  result <- measure(testDataset, 'classValue', c('x1', 'x2'))
  test_that(paste(attr(measure,'name')," returns numeric value"), {
    expect_true(is.numeric(result))
  })
  test_that(paste(attr(measure,'name')," has right kind"), {
    expect_equal(attr(measure,'kind'), "Set measure")
  })
}

for (measure in individualMeasures) {
  test_that(paste(attr(measure,'name')," has right kind"), {
    expect_equal(attr(measure,'kind'), "Individual measure")
  })
}

for (measure in discreteMeasures) {
  test_that(paste(attr(measure,'name')," needs discrete data"), {
    expect_equal(attr(measure,'needsDataToBeDiscrete'), TRUE)
  })
  test_that(paste(attr(measure,'name')," does not need continuous data"), {
    expect_equal(attr(measure,'needsDataToBeContinuous'), FALSE)
  })
  test_that(paste(attr(measure,'name')," throws warning when using a continuous dataset"), {
    expect_warning(measure(iris, 'Species', c('Petal.Width', 'Petal.Length')), 'The data seems not to be discrete, as it should be')
  })
  test_that(paste(attr(measure,'name')," does not throw a warning when using a discrete dataset"), {
    expect_warning(measure(discreteDataset, 'classValue', c('streetNumber', 'doorLetter')), regexp = NA)
  })
}

for (measure in mixedMeasures) {
  test_that(paste(attr(measure,'name')," does not need discrete data"), {
    expect_equal(attr(measure,'needsDataToBeDiscrete'), FALSE)
  })
  test_that(paste(attr(measure,'name')," does not need continuous data"), {
    expect_equal(attr(measure,'needsDataToBeContinuous'), FALSE)
  })
}

for (measure in continuousMeasures) {
  test_that(paste(attr(measure,'name')," needs continuous data"), {
    expect_equal(attr(measure,'needsDataToBeContinuous'), TRUE)
  })
  test_that(paste(attr(measure,'name')," does not need discrete data"), {
    expect_equal(attr(measure,'needsDataToBeDiscrete'), FALSE)
  })
  test_that(paste(attr(measure,'name')," does not throw a warning when using a continuous dataset"), {
    expect_warning(measure(iris, 'Species', c('Petal.Width', 'Petal.Length')), regexp = NA)
  })
}
