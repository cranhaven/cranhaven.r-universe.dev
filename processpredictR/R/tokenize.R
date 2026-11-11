#' Tokenize features and target of a processed dataset of class [`ppred_examples_df`]
#'
#' @description Tokenize features and target of a processed [`ppred_examples_df`] object to fit the Transformer model.
#'
#' @inheritParams split_train_test
#'
#' @return A [`list`] of (sequence) tokens and additional `numeric` or `categorical` features.
#'
#' @export
tokenize <- function(processed_df) {
  UseMethod("tokenize")
}

#' @export
tokenize.ppred_examples_df <- function(processed_df) {

  # vocabulary and task
  vocabulary <- attr(processed_df, "vocabulary")
  task <- attr(processed_df, "task")
  hot_encoded_categorical_features <- attr(processed_df, "hot_encoded_categorical_features")

  # algorithm to produce token_x (same for all tasks)
  token_x <- list()

  for (i in (1:nrow(processed_df))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(processed_df$prefix_list[[i]]))) {
      #if (processed_df$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(processed_df$prefix_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_x <- token_x %>% append(case_trace)
  }


  # time_x (extra features)
  numeric_features <- NULL
  categorical_features <- NULL
  if (!is.null(attr(processed_df, "numeric_features"))) {
    numeric_features <- processed_df %>%
      as_tibble() %>%
      select(attr(processed_df, "numeric_features")) %>%
      data.matrix()
      #scale()

      # as.list() %>%
      # purrr::map(scale) %>%
      # purrr::map(as.vector) %>%
      # as_tibble()
  }

  if (!is.null(attr(processed_df, "hot_encoded_categorical_features"))) {
    categorical_features <- processed_df %>%
      as_tibble() %>%
      select(attr(processed_df, "hot_encoded_categorical_features")) %>%
      data.matrix()
  }

  # if (!is.null(time_x) && !is.null(cat_features)) time_x <- time_x %>% cbind(cat_features) %>% data.matrix()
  # else if (is.null(time_x) && !is.null(cat_features)) time_x <- cat_features %>% data.matrix
  # else if (!is.null(time_x) && is.null(cat_features)) time_x <- time_x %>% data.matrix()


  #algorithm to produce token_y

  # OUTCOME or NEXT_ACTIVITY
  if (task == "outcome") {

    # token_y for OUTCOME
    token_y = c()

    for (i in (1:nrow(processed_df))) {
      tok <- which(processed_df$outcome[i] == vocabulary$keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)
    }

    # # return a list of tokens
    # tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    # class(tokens) <- c("ppred_examples_tokens", "list")
    # attr(tokens, "numeric_features") <- numeric_features
    # attr(tokens, "hot_encoded_categorical_features") <- hot_encoded_categorical_features
    # tokens
  }

  # token_y for NEXT_ACTIVITY
  else if (task == "next_activity") {
    token_y = c()

    for (i in (1:nrow(processed_df))) {
      tok <- which(processed_df$next_activity[i] == vocabulary$keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)
    }

    # # return a list of tokens
    # tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    # class(tokens) <- c("ppred_examples_tokens", "list")
    # attr(tokens, "numeric_features") <- numeric_features
    # attr(tokens, "hot_encoded_categorical_features") <- hot_encoded_categorical_features
    # tokens
  }

  # token_y for REMAINING_TRACE
  else if (task == "remaining_trace") {
    token_y = c()

    for (i in (1:nrow(processed_df))) {
      tok <- which(processed_df$remaining_trace[i] == vocabulary$keys_y)
      token_y <- token_y  %>% append(tok-1)
    }

    # # return a list of tokens:
    # tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    # class(tokens) <- c("ppred_examples_tokens", "list")
    # attr(tokens, "numeric_features") <- numeric_features
    # attr(tokens, "hot_encoded_categorical_features") <- hot_encoded_categorical_features
    # tokens
  }

  # NEXT_TIME and REMAINING_TIME tasks
  else if (task %in% c("next_time", "remaining_time")) {

    # inversing times back to interpret the model predictions output
    # time_passed1 * attr(time_passed1, 'scaled:scale') + attr(time_passed1, 'scaled:center'))

    #time_y (output)
    token_y <- processed_df[[attr(processed_df, "y_var")]] %>% data.matrix()
    #token_y <- processed_df %>% as_tibble() %>% select(attr(processed_df, "y_var")) #%>% #as.list() %>%
      #as.vector()
      #scale() #%>%
      # as.vector()
      # purrr::map(scale) %>%
      # purrr::map(as.vector)

    # # return:
    # # token_x, i.e. activity prefixes
    # # time_x, i.e. a list of calculated and scaled durations (recent, latest, passed)
    # # time_y, i.e. a next_activity duration
    # tokens <- list(token_x = token_x, time_x = time_x, token_y = time_y)
    # class(tokens) <- c("ppred_examples_tokens", "list")
    # attr(tokens, "numeric_features") <- numeric_features
    # attr(tokens, "hot_encoded_categorical_features") <- hot_encoded_categorical_features
    # tokens
  } # followed by train_token_x %>% reticulate::np_array(dtype = "float32") in transformer_fit or predict

  #center_scale <- data.frame(center = )

  tokens <- list(token_x = token_x,
                 numeric_features = numeric_features,
                 categorical_features = categorical_features,
                 token_y = token_y)
  class(tokens) <- c("ppred_examples_tokens", "list")
  attr(tokens, "numeric_features") <- attr(processed_df, "numeric_features")
  attr(tokens, "hot_encoded_categorical_features") <- hot_encoded_categorical_features
  tokens

}


