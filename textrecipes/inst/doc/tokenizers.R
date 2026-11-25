## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(textrecipes)
library(recipes)

## -----------------------------------------------------------------------------
text_tibble <- tibble::tibble(
  text = c(
    "This is a sentence. It has two!",
    "They're e-mailing Dr. Smith about the 3 cats."
  )
)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "words") |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    options = list(lowercase = FALSE, strip_punct = FALSE)
  ) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, options = list(strip_numeric = TRUE)) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "ptb") |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "characters") |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    token = "character_shingle",
    options = list(n = 3)
  ) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "ngrams", options = list(n = 2, n_min = 1)) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(
    text,
    token = "skip_ngrams",
    options = list(n = 2, n_min = 2, k = 1)
  ) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "sentences") |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "regex", options = list(pattern = "[.!]\\s*")) |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
recipe(~text, data = text_tibble) |>
  step_tokenize(text, token = "word_stems") |>
  show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
# recipe(~text, data = text_tibble) |>
#   step_tokenize(
#     text,
#     engine = "tokenizers.bpe",
#     training_options = list(vocab_size = 40)
#   ) |>
#   show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
# recipe(~text, data = text_tibble) |>
#   step_tokenize(text, engine = "spacyr") |>
#   show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
# model <- udpipe::udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
# 
# recipe(~text, data = text_tibble) |>
#   step_tokenize(
#     text,
#     engine = "udpipe",
#     training_options = list(model = model)
#   ) |>
#   show_tokens(text, n = 2)

## -----------------------------------------------------------------------------
space_tokenizer <- function(x) {
  strsplit(x, " +")
}

recipe(~text, data = text_tibble) |>
  step_tokenize(text, custom_token = space_tokenizer) |>
  show_tokens(text, n = 2)

