## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  cache = FALSE
)

## ----setup, message = F, eval = T---------------------------------------------
library(processpredictR)
library(bupaR)
library(ggplot2)
library(dplyr)
library(keras)
library(purrr)

## ----echo = F, eval = T, out.width = "60%", fig.align = "center"--------------
knitr::include_graphics("framework.PNG")

## ----eval = T-----------------------------------------------------------------
df <- prepare_examples(traffic_fines, task = "outcome")
df

## ----eval = T-----------------------------------------------------------------
set.seed(123)
split <- df %>% split_train_test(split = 0.8)
split$train_df %>% head(5)
split$test_df %>% head(5)

## ----eval = T-----------------------------------------------------------------
nrow(split$train_df) / nrow(df)
n_distinct(split$train_df$case_id) / n_distinct(df$case_id)

## -----------------------------------------------------------------------------
# model <- split$train_df %>% create_model(name = "my_model")
# # pass arguments as ... that are applicable to keras::keras_model()
# 
# model # is a list

## -----------------------------------------------------------------------------
# model %>% names() # objects from a returned list

## -----------------------------------------------------------------------------
# model$model$name # get the name of a model

## -----------------------------------------------------------------------------
# model$model$non_trainable_variables # list of non-trainable parameters of a model

## -----------------------------------------------------------------------------
# model %>% compile() # model compilation

## -----------------------------------------------------------------------------
# hist <- fit(object = model, train_data = split$train_df, epochs = 5)

## -----------------------------------------------------------------------------
# hist$params

## -----------------------------------------------------------------------------
# hist$metrics

## -----------------------------------------------------------------------------
# predictions <- model %>% predict(test_data = split$test_df,
#                                  output = "append") # default
# predictions %>% head(5)

## -----------------------------------------------------------------------------
# predictions %>% class

## -----------------------------------------------------------------------------
# confusion_matrix(predictions)

## ----out.width="100%", fig.width = 7------------------------------------------
# plot(predictions) +
#   theme(axis.text.x = element_text(angle = 90))

## ----out.width="100%", fig.width = 7------------------------------------------
# knitr::include_graphics("confusion_matrix.PNG")

## -----------------------------------------------------------------------------
# model %>% evaluate(split$test_df)

## -----------------------------------------------------------------------------
# # preprocessed dataset with categorical hot encoded features
# df_next_time <- traffic_fines %>%
#   group_by_case() %>%
#   mutate(month = lubridate::month(min(timestamp), label = TRUE)) %>%
#   ungroup_eventlog() %>%
#   prepare_examples(task = "next_time", features = "month") %>% split_train_test()
# 
# 

## -----------------------------------------------------------------------------
# # the attributes of df are added or changed accordingly
# 
# df_next_time$train_df %>% attr("features")

## -----------------------------------------------------------------------------
# df_next_time$train_df %>% attr("hot_encoded_categorical_features")

## -----------------------------------------------------------------------------
# df <- prepare_examples(traffic_fines, task = "next_activity") %>% split_train_test()
# custom_model <- df$train_df %>% create_model(custom = TRUE, name = "my_custom_model")
# custom_model

## -----------------------------------------------------------------------------
# custom_model <- custom_model %>%
#   stack_layers(layer_dropout(rate = 0.1)) %>%
#   stack_layers(layer_dense(units = 64, activation = 'relu'))
# custom_model

## -----------------------------------------------------------------------------
# # this works too
# custom_model %>%
#   stack_layers(layer_dropout(rate = 0.1), layer_dense(units = 64, activation = 'relu'))

## -----------------------------------------------------------------------------
# new_outputs <- custom_model$model$output %>% # custom_model$model to access a model and $output to access the outputs of that model
#   keras::layer_dropout(rate = 0.1) %>%
#   keras::layer_dense(units = custom_model$num_outputs, activation = 'softmax')
# 
# custom_model <- keras::keras_model(inputs = custom_model$model$input, outputs = new_outputs, name = "new_custom_model")
# custom_model
# 

## -----------------------------------------------------------------------------
# # class of the model
# custom_model %>% class

## -----------------------------------------------------------------------------
# # compile
# compile(object=custom_model, optimizer = "adam",
#         loss = loss_sparse_categorical_crossentropy(),
#         metrics = metric_sparse_categorical_crossentropy())

## -----------------------------------------------------------------------------
# # the trace of activities must be tokenized
# tokens_train <- df$train_df %>% tokenize()
# map(tokens_train, head) # the output of tokens is a list
# 
# 

## -----------------------------------------------------------------------------
# # make sequences of equal length
# x <- tokens_train$token_x %>% pad_sequences(maxlen = max_case_length(df$train_df), value = 0)
# y <- tokens_train$token_y

## ----eval=F-------------------------------------------------------------------
# # train
# fit(object = custom_model, x, y, epochs = 10, batch_size = 10) # see also ?keras::fit.keras.engine.training.Model
# 
# # predict
# tokens_test <- df$test_df %>% tokenize()
# x <- tokens_test$token_x %>% pad_sequences(maxlen = max_case_length(df$train_df), value = 0)
# predict(custom_model, x)
# 
# # evaluate
# tokens_test <- df$test_df %>% tokenize()
# x <- tokens_test$token_x
# # normalize by dividing y_test over the standard deviation of y_train
# y <- tokens_test$token_y / sd(tokens_train$token_y)
# evaluate(custom_model, x, y)

