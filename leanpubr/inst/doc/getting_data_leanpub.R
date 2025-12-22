## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------
Sys.setenv("LEANPUB_API_KEY" = "asdf")

## -----------------------------------------------------------------------------
library(leanpubr)
slug = "biostatmethods"
res = lp_summary(slug, error = FALSE, verbose = TRUE)
res$content

