## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(textrecipes)
library(recipes)

## -----------------------------------------------------------------------------
toy_embedder <- function(x) {
  cbind(
    nchar = nchar(x),
    vowels = nchar(gsub("[^aeiou]", "", x))
  )
}

sample_data <- tibble::tibble(
  text = c("The cat sat.", "A dog ran.", "Stocks fell sharply today.")
)

rec <- recipe(~text, data = sample_data) |>
  step_sentence_embeddings(text, engine = embedding_custom(toy_embedder))

rec |>
  prep() |>
  bake(new_data = NULL)

## -----------------------------------------------------------------------------
# rec <- recipe(~text, data = sample_data) |>
#   step_sentence_embeddings(
#     text,
#     engine = embedding_ollama("all-minilm")
#   )

## -----------------------------------------------------------------------------
# rec <- recipe(~text, data = sample_data) |>
#   step_sentence_embeddings(
#     text,
#     engine = embedding_huggingface(
#       "sentence-transformers/all-MiniLM-L6-v2",
#       revision = "1110a243fdf4706b3f48f1d95db1a4f5529b4d41",
#       device = "cpu"
#     )
#   )

## -----------------------------------------------------------------------------
# # Fine to save, the recipe holds the engine settings, not the model
# rec_prepped <- prep(rec)
# saveRDS(rec_prepped, "recipe.rds")

