## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(textrecipes)
library(recipes)

## -----------------------------------------------------------------------------
train <- tibble::tibble(
  text = c(
    "the cat sat on the mat",
    "the dog chased the cat",
    "dogs and cats are pets",
    "a mat is a small rug"
  )
)

new_docs <- tibble::tibble(
  text = c("the cat chased a small dog", "a rug for the mat")
)

## -----------------------------------------------------------------------------
rec <- recipe(~text, data = train) |>
  step_tokenize(text) |>
  step_tfidf(text, sparse = "yes") |>
  prep()

## -----------------------------------------------------------------------------
x_train <- bake(rec, new_data = NULL, composition = "dgCMatrix")
x_train

## -----------------------------------------------------------------------------
x_new <- bake(rec, new_data = new_docs, composition = "dgCMatrix")
x_new

identical(colnames(x_train), colnames(x_new))

## -----------------------------------------------------------------------------
# # Fit a lasso model with glmnet
# fit <- glmnet::glmnet(x_train, y_train, family = "binomial")
# predict(fit, newx = x_new)

## -----------------------------------------------------------------------------
# recipe(~text, data = large_corpus) |>
#   step_tokenize(text) |>
#   step_tokenfilter(text, max_tokens = 5000) |>
#   step_tf(text, sparse = "yes") |>
#   prep() |>
#   bake(new_data = NULL, composition = "dgCMatrix")

## -----------------------------------------------------------------------------
# recipe(~text, data = large_corpus) |>
#   step_tokenize(text) |>
#   step_texthash(text, num_terms = 4096, sparse = "yes") |>
#   prep() |>
#   bake(new_data = NULL, composition = "dgCMatrix")

