# Helper for cross-validation: return the next test indices.
get_testIndices <- function(n, CV, v, shuffle_inds) {
  if (CV$type == "vfold") {
    # Slice indices (optionnally shuffled)
    first_index = round((v-1) * n / CV$V) + 1
    last_index = round(v * n / CV$V)
    test_indices = first_index:last_index
    if (!is.null(shuffle_inds))
      test_indices <- shuffle_inds[test_indices]
  }
  else
    # Monte-Carlo cross-validation
    test_indices = sample(n, round(n * CV$test_size))
  test_indices
}

# Helper which split data into training and testing parts.
splitTrainTest <- function(data, target, testIdx) {
  dataTrain <- data[-testIdx,]
  targetTrain <- target[-testIdx]
  dataTest <- data[testIdx,]
  targetTest <- target[testIdx]
  # [HACK] R will cast 1-dim matrices into vectors:
  if (!is.matrix(dataTrain) && !is.data.frame(dataTrain))
    dataTrain <- as.matrix(dataTrain)
  if (!is.matrix(dataTest) && !is.data.frame(dataTest))
    dataTest <- as.matrix(dataTest)
  list(dataTrain=dataTrain, targetTrain=targetTrain,
       dataTest=dataTest, targetTest=targetTest)
}
