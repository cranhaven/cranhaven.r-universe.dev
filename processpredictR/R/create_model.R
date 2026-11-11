#' Define transformer model
#'
#' Defines the model using the keras functional API.
#'
#' @param x_train [`data.frame`]: A processed [`data.frame`] from `prepare_examples()`.
#' @param custom [`logical`] (default [`FALSE`]): If [`TRUE`], returns a custom model.
#' @param ... you can pass additional arguments to `keras::keras_model()` (ex.: `name` argument).
#' @return A [`list`] containing a Transformer model (returned by `keras::keras_model()`) and some additional useful metrics.
#'
#' @export
create_model <- function(x_train, custom = FALSE, ...) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(x_train, custom = FALSE, ...) {

  # parameters of the model
  task <- attr(x_train, "task")
  features <- attr(x_train, "features")
  num_features <- features %>% length() %>% as.integer()
  numeric_features <- attr(x_train, "numeric_features")
  categorical_features <- attr(x_train, "hot_encoded_categorical_features")
  # number_numeric_features <- attr(x_train, "numeric_features") %>% length() %>% as.integer()
  # number_categorical_features <- attr(x_train, "hot_encoded_categorical_features") %>% length() %>% as.integer()
  num_outputs <- attr(x_train, "num_outputs")
  max_case_length <- attr(x_train, "max_case_length") %>% as.integer()
  vocab_size <- attr(x_train, "vocab_size") %>% as.integer()
  embed_dim <- 36 %>% as.integer()
  num_heads <- 4 %>% as.integer()
  ff_dim <- 64 %>% as.integer()


# Initialize transformer model --------------------------------------------
  inputs <- keras::layer_input(shape = c(max_case_length))
  outputs <- inputs %>%
    TokenAndPositionEmbedding()(maxlen = max_case_length, vocab_size = vocab_size, embed_dim = embed_dim) %>%
    TransformerBlock()(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
    keras::layer_global_average_pooling_1d() #%>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = 64, activation = 'relu') %>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = num_outputs, activation = 'linear')

# extra features ----------------------------------------------------------
  if (!is.null(numeric_features)) {
    numeric_features_train <- x_train %>%
      as_tibble() %>%
      select(numeric_features) %>% data.matrix()

    #}

    #if (number_numeric_features > 0) {
    #number_numeric_features <- numeric_features %>% length() %>% as.integer()
    number_numeric_features <- numeric_features_train %>% colnames() %>% length() %>% as.integer()

    numeric_inputs <- keras::layer_input(shape = c(number_numeric_features))
    scale_numeric <- keras::layer_normalization()
    scale_numeric %>% adapt(numeric_features_train)
    numeric_outputs <- numeric_inputs %>%
      scale_numeric() %>%
      keras::layer_dense(units = 32, activation = "relu")

    outputs <- keras::layer_concatenate(list(outputs, numeric_outputs))
  }
  else numeric_inputs <- NULL

  #if (number_categorical_features > 0) {
  if (!is.null(categorical_features)) {
    categorical_features_train <- x_train %>%
      as_tibble() %>%
      select(categorical_features) %>% data.matrix()

    #number_categorical_features <- categorical_features %>% length() %>% as.integer()
    number_categorical_features <- categorical_features_train %>% colnames() %>% length() %>% as.integer()
    categorical_inputs <- keras::layer_input(shape = c(number_categorical_features))
    categorical_outputs <- categorical_inputs %>%
      keras::layer_dense(units = 32, activation = "relu") # change units ????

    outputs <- keras::layer_concatenate(list(outputs, categorical_outputs))
  }
  else categorical_inputs <- NULL


# finalize default model --------------------------------------------------
  if (!custom) {
    outputs <- outputs %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = 64, activation = 'relu') %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = num_outputs, activation = if_else(task %in% c("next_time", "remaining_time"), "relu", "softmax"))
  }

  #if (num_features > 0) {
    list_inputs <- list(inputs) %>% append(numeric_inputs) %>% append(categorical_inputs)
    model <- keras::keras_model(inputs = list_inputs, outputs = outputs, ...)
  #}
  #else {
    #model <- keras::keras_model(inputs = inputs, outputs = outputs)
  #}


# assign attributes -------------------------------------------------------
 output <- list()
 class(output) <- c("ppred_model", class(output))
 output$model <- model
 # Attributes now stored as list-object components
 output$max_case_length <- max_case_length
 output$features <- features
 output$number_features <- num_features
 output$task <- task
 output$num_outputs <- num_outputs
 output$numeric_features <- numeric_features
 output$categorical_features <- categorical_features
 output$vocabulary <- attr(x_train, "vocabulary")

 if (task %in% c("next_time", "remaining_time")) {

   y_token_train <- x_train %>%
     as_tibble() %>%
     pull(attr(x_train, "y_var")) #%>% data.matrix()

   #######################   #######################   #######################   #######################

   sd_time <- sd(y_token_train)
   output$sd_time <- sd_time

   # y_token_train <- x_train %>%
   #   as_tibble() %>%
   #   select(attr(x_train, "y_var")) %>% data.matrix() FOR OUTCOME WAS VERY INTERESTING

   # # original
   # normalize_y <- keras::layer_normalization()
   # normalize_y %>% adapt(y_token_train)
   # output$y_normalize_layer <- normalize_y
 }

 #######################   #######################   #######################   #######################

 # Attributes for temporary backwards compatitibility
  attr(output, "max_case_length") <- max_case_length
  attr(output, "features") <- features
  attr(output, "number_features") <- num_features
  attr(output, "task") <- task

  output
}





