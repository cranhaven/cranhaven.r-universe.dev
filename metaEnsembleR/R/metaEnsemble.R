#############################
# For Classification
#############################

ensembler.classifier <- function(data, outcomeVARIABLEINDEX, IndividualModels,  TopLayerModel, dstr,dsv, dst, unseen_new_data) {
  predictors <- names(data[,-c(outcomeVARIABLEINDEX)])
  print(predictors)
  fractionTraining   <- dstr
  fractionValidation <- dsv
  fractionTest       <- dst

  # Compute sample sizes.
  sampleSizeTraining   <- floor(fractionTraining   * nrow(data))
  sampleSizeValidation <- floor(fractionValidation * nrow(data))
  sampleSizeTest       <- floor(fractionTest       * nrow(data))

  # Create the randomly-sampled indices for the dataframe. Use setdiff() to
  # avoid overlapping subsets of indices.
  indicesTraining    <- sort(sample(seq_len(nrow(data)), size=sampleSizeTraining))
  indicesNotTraining <- setdiff(seq_len(nrow(data)), indicesTraining)
  indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
  indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

  # Finally, output the three data frames for training, validation and test.
  Training   <- data[indicesTraining, ]
  Validation <- data[indicesValidation, ]
  Test       <- data[indicesTest, ]

  print(nrow(Training))
  print(nrow(Validation))
  print(nrow(Test))

  for (i in 1:length(IndividualModels)){
    model <- caret::train(Training[,predictors], Training[,outcomeVARIABLEINDEX], method=IndividualModels[i])
    print(model)
    Training[, (ncol(Training)+1)] <- (caret::predict.train(object=model, (Training[,predictors])))
    print(Training[, (ncol(Training))])

    Validation[,(ncol(Validation)+1)] <- (caret::predict.train(object=model, (Validation[,predictors])))



    Test[,(ncol(Test)+1)] <- (caret::predict.train(object=model, (Test[,predictors])))


  }

  predictors <- names(data[,-c(outcomeVARIABLEINDEX)])
  print(predictors)
  model_final <- caret::train(Training[,predictors], Training[,outcomeVARIABLEINDEX], method=TopLayerModel) #training
  print(model_final)
  predictions <- caret::predict.train(object=model_final, Test[,predictors])
  print(predictions)
  actual_label <-  Test[,outcomeVARIABLEINDEX]
  testpreddata <- cbind(Test[,predictors], predictions,  actual_label )
  model_results <- caret::postResample(pred = predictions, obs = actual_label)
  unseenpreddata <- caret::predict.train(object=model_final, unseen_new_data[,-c(outcomeVARIABLEINDEX)]) #unseen_new_data[,predictors]
  unseenpreddata <- cbind(unseen_new_data[,predictors], unseenpreddata)
  return(list(testpreddata, predictions, model_results, unseenpreddata))
}

#############################
# For Regression
#############################


ensembler.regression <- function(data, outcomeVARIABLEINDEX, IndividualModels,  TopLayerModel, dstr,dsv, dst, unseen_new_data) {
  predictors <- names(data[,-c(outcomeVARIABLEINDEX)])
  print(predictors)
  fractionTraining   <- dstr
  fractionValidation <- dsv
  fractionTest       <- dst

  # Compute sample sizes.
  sampleSizeTraining   <- floor(fractionTraining   * nrow(data))
  sampleSizeValidation <- floor(fractionValidation * nrow(data))
  sampleSizeTest       <- floor(fractionTest       * nrow(data))

  # Create the randomly-sampled indices for the dataframe. Use setdiff() to
  # avoid overlapping subsets of indices.
  indicesTraining    <- sort(sample(seq_len(nrow(data)), size=sampleSizeTraining))
  indicesNotTraining <- setdiff(seq_len(nrow(data)), indicesTraining)
  indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
  indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

  # Finally, output the three dataframes for training, validation and test.
  Training   <- data[indicesTraining, ]
  Validation <- data[indicesValidation, ]
  Test       <- data[indicesTest, ]

  print(nrow(Training))
  print(nrow(Validation))
  print(nrow(Test))


  for (i in 1:length(IndividualModels)){
    model <- caret::train(Training[,predictors], Training[,outcomeVARIABLEINDEX], method=IndividualModels[i])
    print(model)
    Training[, (ncol(Training)+1)] <- (caret::predict.train(object=model, (Training[,predictors])))
    print(Training[, (ncol(Training))])

    Validation[,(ncol(Validation)+1)] <- (caret::predict.train(object=model, (Validation[,predictors])))



    Test[,(ncol(Test)+1)] <- (caret::predict.train(object=model, (Test[,predictors])))


  }

  predictors <- names(data[,-c(outcomeVARIABLEINDEX)])
  print(predictors)
  model_final <- caret::train(Training[,predictors], Training[,outcomeVARIABLEINDEX], method=TopLayerModel) #training
  print(model_final)
  predictions <- caret::predict.train(object=model_final, Test[,predictors])
  print(predictions)
  actual_label <-  Test[,outcomeVARIABLEINDEX]
  testpreddata <- cbind(Test[,predictors], predictions,  actual_label )
  model_results <- caret::postResample(pred = predictions, obs = actual_label)
  unseenpreddata <- caret::predict.train(object=model_final, unseen_new_data[,-c(outcomeVARIABLEINDEX)]) #unseen_new_data[,predictors]
  unseenpreddata <- cbind(unseen_new_data[,predictors], unseenpreddata)
  return(list(testpreddata, predictions,model_results, unseenpreddata))
}

