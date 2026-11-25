## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(textrecipes)
# library(recipes)

## -----------------------------------------------------------------------------
# repo <- "minishlab/potion-base-8M"
# commit <- "bf8b056651a2c21b8d2565580b8569da283cab23"
# url <- paste0("https://huggingface.co/", repo, "/resolve/", commit)
# 
# model_dir <- download_embeddings_model2vec(url)

## -----------------------------------------------------------------------------
# embeddings <- read_embeddings_model2vec(model_dir)
# vocab <- readLines(file.path(model_dir, "vocab.txt"), warn = FALSE)

## -----------------------------------------------------------------------------
# sample_data <- tibble::tibble(
#   text = c(
#     "The cat sat on the mat.",
#     "Unbelievably good tokenization!"
#   )
# )
# 
# rec <- recipe(~text, data = sample_data) |>
#   step_tokenize_wordpiece(text, vocab = vocab) |>
#   step_word_embeddings(text, embeddings = embeddings, aggregation = "mean") |>
#   step_spatialsign(starts_with("wordembed_"))
# 
# rec |>
#   prep() |>
#   bake(new_data = NULL)

## -----------------------------------------------------------------------------
# url <- "https://bpemb.h-its.org/en"
# 
# bpemb_dir <- tempfile()
# dir.create(bpemb_dir)
# download.file(
#   file.path(url, "en.wiki.bpe.vs10000.model"),
#   file.path(bpemb_dir, "en.model"),
#   mode = "wb"
# )
# download.file(
#   file.path(url, "en.wiki.bpe.vs10000.d50.w2v.txt.tar.gz"),
#   file.path(bpemb_dir, "vectors.tar.gz"),
#   mode = "wb"
# )
# untar(file.path(bpemb_dir, "vectors.tar.gz"), exdir = bpemb_dir)

## -----------------------------------------------------------------------------
# vectors_file <- list.files(
#   bpemb_dir,
#   pattern = "w2v.txt$",
#   recursive = TRUE,
#   full.names = TRUE
# )
# 
# embeddings <- read_embeddings_bpemb(vectors_file)

## -----------------------------------------------------------------------------
# sample_data <- tibble::tibble(
#   text = c(
#     "The cat sat on the mat.",
#     "Unbelievably good tokenization in 2024!"
#   )
# )
# 
# rec <- recipe(~text, data = sample_data) |>
#   step_mutate(text = gsub("\\p{Nd}", "0", tolower(text), perl = TRUE)) |>
#   step_tokenize_sentencepiece(text, model = file.path(bpemb_dir, "en.model")) |>
#   step_word_embeddings(text, embeddings = embeddings, aggregation = "mean")
# 
# rec |>
#   prep() |>
#   bake(new_data = NULL)

## -----------------------------------------------------------------------------
# tokens <- recipe(~text, data = sample_data) |>
#   step_mutate(text = gsub("\\p{Nd}", "0", tolower(text), perl = TRUE)) |>
#   step_tokenize_sentencepiece(text, model = file.path(bpemb_dir, "en.model")) |>
#   show_tokens(text, n = 2)
# 
# all(unlist(tokens) %in% embeddings$token)

## -----------------------------------------------------------------------------
# library(wordsalad)
# 
# embeddings <- word2vec(
#   fairy_tales,
#   tokenizer = tokenizers::tokenize_words,
#   dim = 25,
#   min_count = 5L,
#   n_iter = 10L
# )
# 
# sample_data <- tibble::tibble(
#   text = c(
#     "The king and the queen went to the castle.",
#     "A little girl found a magic key."
#   )
# )
# 
# rec <- recipe(~text, data = sample_data) |>
#   step_tokenize(text) |>
#   step_word_embeddings(text, embeddings = embeddings, aggregation = "mean")
# 
# rec |>
#   prep() |>
#   bake(new_data = NULL)

