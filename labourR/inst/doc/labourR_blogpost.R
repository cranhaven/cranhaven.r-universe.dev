## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
library(labourR)
corpus <- data.frame(
  id = 1:3,
  text = c("Data Scientist", "Junior Architect Engineer", "Cashier at McDonald's")
)

## ----with_isco_level----------------------------------------------------------
classify_occupation(corpus = corpus, isco_level = 3, lang = "en", num_leaves = 5)

