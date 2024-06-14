#'@title Perform Ensemble modelling using Caret Package.
#'
#'@description Take any number of Machine Learning Models as Base Models and one Machine Learning Model as Top model to produce an ensemble of models.
#'
#'@param training a dataset containing the outcome varable with all other independent variables.
#'
#'@param testing a dataset containing all the independent variable columns as well as outcome variable column with NULL values i.e. must have same no. of columns as the training dataset
#'
#'@param outcomeName the column name associated with outcome variable in the training dataset.
#'
#'@param BaseModels character string vector containing the names of all base models as in 'caret package' desired to be used for ensembling.
#'
#'@param TopModel name of the model as in 'caret package' that is wished to be used on top.
#'
#'@return predictions the estimated outcome variables column for testing dataset.
#'
#'@examples
#' data(iris)
#' preds <- ensemble(iris[1:125,],iris[125:150,],'Species',c('treebag','rpart'),'rpart')
#' table(preds)
#'
#'@export

ensemble<-function(training,testing,outcomeName,BaseModels,TopModel)
  {



  predictors <- names(training)[!names(training) %in% outcomeName]


  for(i in 1:length(BaseModels))
  {

    model <- caret::train(training[,predictors], training[,outcomeName], method=BaseModels[i])


    testing[,(ncol(testing)+1)] <- caret::predict.train(object=model, testing[,predictors])



    training[,(ncol(training)+1)] <- caret::predict.train(object=model, training[,predictors])

   }




  predictors <- names(training)[!names(training) %in% outcomeName]

  model_final <- caret::train(training[,predictors], training[,outcomeName], method=TopModel)

  predictions <- caret::predict.train(object=model_final, testing[,predictors])

  return(predictions)

  }
