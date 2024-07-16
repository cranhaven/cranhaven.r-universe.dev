#' Train a three layers neural network model.
#'
#' @param train A dataframe with `class` column as label.
#' @param test A dataframe with `class` column as label.
#' @param path2save The folder path to store the model and train history.
#' @param batch_size Integer or NULL.Number of samples per gradient update.
#' @param epochs Number of epochs to train the model.
#' @param cls A character.The name of the label column.
#' @param validation_split Float between 0 and 1. Fraction of the training data to be used as validation data.
#'
#' @importFrom dplyr rename mutate across select
#' @importFrom keras to_categorical layer_activation_relu layer_dense keras_model_sequential save_model_tf k_argmax evaluate optimizer_adam fit compile
#' @importFrom caret confusionMatrix
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom stringr str_c
#' @return A `list` object th at contains the prediction confusion matrix and the `model` object.
#' @export
mi_train_BP <- function(train, test, cls = "class", path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3) {
  train <- train %>%
    rename("class" = cls)
  test <- test %>%
    rename("class" = cls)
  train_set <- train %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()
  test_set <- test %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()

  train_target <- train$class %>%
    as.numeric() %>%
    to_categorical() %>%
    magrittr::set_names(levels(test$class))
  test_target <- test$class %>%
    as.numeric() %>%
    to_categorical() %>%
    magrittr::set_names(levels(test$class))
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 40, input_shape = ncol(train_set)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 40) %>%
    layer_activation_relu() %>%
    layer_dense(units = ncol(train_target), activation = "softmax")
  summary(model)

  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = list("categorical_accuracy")
  )
  history <- model %>% fit(
    x = train_set, y = train_target,
    epochs = epochs, batch_size = batch_size,
    validation_split = 0.3, verbose = 2
  )
  if(!is.null(path2save)){
    save_model_tf(model, str_c(path2save, "/result_net"))
  }
  predictions <- predict(model, test_set)
  response <- predictions %>% k_argmax()
  response <- response$numpy() %>%
    as.numeric() %>%
    levels(test$class)[.] %>%
    factor(levels = levels(test$class))
  prd_net <- confusionMatrix(response, test$class)
  score <- model %>% evaluate(test_set, test_target)
  return(list(model, prd_net))
}
