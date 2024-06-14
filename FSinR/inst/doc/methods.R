## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ---- 'Simple'----------------------------------------------------------------
library(FSinR)

sequentialForwardSelection()(iris, 'Species', IEConsistency())
sequentialForwardSelection()(iris, 'Species', mutualInformation())

## ---- 'List of measures'------------------------------------------------------
measures <- list(IEConsistency(), mutualInformation())
for (measure in measures) {
  result <- sequentialForwardSelection()(iris, 'Species', measure)
  print(attr(measure,'name'))
  print(result$bestFeatures)
}

## ---- 'List of algorithms'----------------------------------------------------
measures <- list(IEConsistency(), mutualInformation())
algorithms <- list(sequentialForwardSelection(), LasVegas())
for (algorithm in algorithms) {
  for (measure in measures) {
    result <- algorithm(iris, 'Species', measure)
    print(paste("Algorithm: ",attr(algorithm,'name')))
    print(paste("Evaluation measure: ", attr(measure,'name')))
    print(result$bestFeatures)
  }
}

## ---- 'Add wrapper'-----------------------------------------------------------
resamplingParams <- list(method = "cv", number = 10)
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = c(1:20)))

wra <- wrapperEvaluator("knn", resamplingParams, fittingParams)

measures <- list(IEConsistency(), mutualInformation(), wra)

