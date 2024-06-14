resamplingParams <- list(method = "cv", number = 5)
fittingParams <- list(preProc = c("center", "scale"), metric = "Accuracy", tuneGrid = expand.grid(k = seq(1, 10, by = 2)))
wra <- wrapperEvaluator("knn",resamplingParams, fittingParams) # wrapper method
measures <- list(binaryConsistency(), IEConsistency(), IEPConsistency(), roughsetConsistency(), giniIndex(), mutualInformation(), gainRatio(), symmetricalUncertain(), determinationCoefficient(), MDLC(), ReliefFeatureSetMeasure(), wra)

methods <- list(
  list(method = sequentialForwardSelection(),
       name = 'SFS'),
  list(method = sequentialBackwardSelection(),
       name = 'SBS'),
  list(method = sequentialFloatingForwardSelection(),
       name = 'SFFS'),
  list(method = sequentialFloatingBackwardSelection(),
       name = 'SFBS'),
  list(method = LasVegas(),
       name = 'LVW'),
  list(method = geneticAlgorithm(),
       name = 'GA'),
  list(method = simulatedAnnealing(),
       name = 'SA'),
  list(method = whaleOptimization(),
       name = 'WOA'),
  list(method = antColony(),
       name = 'ACO'),
  list(method = tabu(),
       name = 'TS'),
  list(method = hillClimbing(),
       name = 'HC')
)
for (method in methods) {
  print("")
  print("###############################################")
  print(paste("Method ",method$name))
  print("###############################################")
  print("")
  print(paste("Method ",method$name))
  for (measure in measures) {
    print("")
    print("   @@@@@@@")
    print(paste("   @Measure ",attr(measure,'name')))
    print("   @@@@@@@")
    print("")
    value <- method$method(iris, 'Species',measure)
    print(value)
  }
}
