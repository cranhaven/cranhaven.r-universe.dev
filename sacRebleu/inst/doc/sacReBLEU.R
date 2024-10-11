## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(sacRebleu)
cand_corpus <- list(c(1,2,3), c(1,2))
ref_corpus <- list(list(c(1,2,3), c(2,3,4)), list(c(1,2,6), c(781, 21, 9), c(7, 3)))
bleu_corpus_ids_standard <- bleu_corpus_ids(ref_corpus, cand_corpus)

