#' Random Forest Model Training.
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#' @param instance A tuner.
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @return  A list of learner for predict and predict result of test set.
#' @export
mi_train_rg <- function(train, test, measure = msr("classif.acc"),instance = NULL) {
  learner <- lrn("classif.ranger",importance='impurity',
                 regularization.factor=0.01407, minprop=0.01667, num.trees=385,
                 max.depth=368)
  if(!is.null(instance)){
    learner$param_set$values = instance$result_learner_param_vals
  }
  task_train <- train %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"))
  task_predict <- test %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"))
  train_set <- partition(task_train, ratio = 1)$train
  test_set <- partition(task_predict, ratio = 0)$test
  set_threads(learner)
  learner$train(task_train, row_ids = train_set)
  predict <- learner$predict(task_predict, row_ids = test_set)
  list(learner, predict)
}
