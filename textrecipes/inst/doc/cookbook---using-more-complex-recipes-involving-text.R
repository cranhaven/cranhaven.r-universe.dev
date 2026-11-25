## -----------------------------------------------------------------------------
if (rlang::is_installed(c("modeldata"))) {
  run <- TRUE
} else {
  run <- FALSE
}

knitr::opts_chunk$set(
  eval = run,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(recipes)
library(textrecipes)
library(modeldata)
data("tate_text")

## -----------------------------------------------------------------------------
words <- c("or", "and", "on")

okc_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stopwords(medium, custom_stopword_source = words, keep = TRUE) |>
  step_tf(medium)

okc_obj <- okc_rec |>
  prep()

bake(okc_obj, tate_text) |>
  select(starts_with("tf_medium"))

## -----------------------------------------------------------------------------
stopwords_list <- c(
  "was", "she's", "who", "had", "some", "same", "you", "most",
  "it's", "they", "for", "i'll", "which", "shan't", "we're",
  "such", "more", "with", "there's", "each"
)

words <- c("sad", "happy")

okc_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  step_stopwords(medium, custom_stopword_source = stopwords_list) |>
  step_stopwords(medium, custom_stopword_source = words) |>
  step_tfidf(medium)

okc_obj <- okc_rec |>
  prep()

bake(okc_obj, tate_text) |>
  select(starts_with("tfidf_medium"))

## -----------------------------------------------------------------------------
okc_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium, token = "characters") |>
  step_stopwords(medium, custom_stopword_source = letters, keep = TRUE) |>
  step_tf(medium)

okc_obj <- okc_rec |>
  prep()

bake(okc_obj, tate_text) |>
  select(starts_with("tf_medium"))

## -----------------------------------------------------------------------------
okc_rec <- recipe(~., data = tate_text) |>
  step_tokenize(medium, token = "words") |>
  step_stem(medium) |>
  step_untokenize(medium) |>
  step_tokenize(medium, token = "ngrams") |>
  step_tokenfilter(medium, max_tokens = 500) |>
  step_tfidf(medium)

okc_obj <- okc_rec |>
  prep()

bake(okc_obj, tate_text) |>
  select(starts_with("tfidf_medium"))

