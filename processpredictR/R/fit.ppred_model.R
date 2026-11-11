#' @title Default fit function for ProcessTransformer model
#' @param object Object of class [ppred_model]: A transformer model
#' @param train_data A training dataset
#' @param batch_size A batch size
#' @param num_epochs  A number of epochs
#' @param verbose A verbose
#' @param callbacks [`list`]: A list of callbacks
#' @param shuffle [`logical`] (default [`TRUE`]): If [`TRUE`] shuffles the data
#' @param validation_split A ratio to split on
#' @param ... Additional Arguments
#'
#' @importFrom keras fit
#' @export
keras::fit

#' @export
fit.ppred_model <- function(object,
                            train_data,
                            batch_size = 10,
                            epochs,
                            verbose = 1,
                            callbacks = list(keras::callback_csv_logger(filename = paste("log_", object$task)), #or NULL
                                             keras::callback_tensorboard()),
                            shuffle = TRUE,
                            validation_split = 0.2,
                            ...) {


# preparation -------------------------------------------------------------
  # parameters
  maxlen <- object$max_case_length %>% as.integer()
  num_epochs <- epochs %>% as.integer()
  batch_size <- batch_size %>% as.integer()

  # tokenizing
  tokens_train <- tokenize(train_data)

  # preparring x_train, y_train for keras::fit()
  x_token_train <- tokens_train$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  #x_token_train <- x_token_train #%>% reticulate::np_array(dtype = "float32")
  x_numeric_features <- tokens_train$numeric_features
  x_categorical_features <- tokens_train$categorical_features

  x_train_list <- list(x_token_train)
  if (!is.null(x_numeric_features)) x_train_list <- x_train_list %>% append(list(x_numeric_features))
  if (!is.null(x_categorical_features)) x_train_list <- x_train_list %>% append(list(x_categorical_features))
  y_token_train <- tokens_train$token_y #%>% reticulate::np_array(dtype = "float32")

  if (object$task %in% c("next_time", "remaining_time")) {
    #######################    #######################    #######################    #######################
    # y_token_train <- object$y_normalize_layer(y_token_train)
    y_token_train <- y_token_train / object$sd_time
    #######################    #######################    #######################    #######################

    # normalize_y <- keras::layer_normalization(mean = as.double(object$y_normalize_layer$mean),
    #                                           variance = as.double(object$y_normalize_layer$variance))
    #normalize_y %>% adapt(y_token_train)
    #y_token_train <- normalize_y(y_token_train)
  }
# keras::fit --------------------------------------------------------------
  #if (object$number_features > 0) {
    keras::fit(object$model,
               #x = list(x_token_train, x_features_train),
               x = x_train_list,
               y = y_token_train,
               batch_size = batch_size,
               epochs = epochs,
               verbose = verbose,
               callbacks = callbacks,
               shuffle = shuffle,
               validation_split = validation_split,
               ...
    )

  # model$keras.engine.functional.Functional$y_normalization_layer <- normalize_y
  # model

  # output <- list()
  # output$model <- model
  # output$y_normalization_layer <- normalize_y
  # return(output)

  #}
  # else {
  #   keras::fit(object$model,
  #              x = x_token_train,
  #              y = y_token_train,
  #              batch_size = batch_size,
  #              epochs = epochs,
  #              verbose = verbose,
  #              callbacks = callbacks,
  #              shuffle = shuffle,
  #              validation_split = validation_split,
  #              ...
  #   )
  # }

}
