## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----pressure, echo=FALSE, fig.cap="Fig.1 - ESCO is mapped to the 4th level of the ISCO hierarchical model.", out.width = '100%'----
knitr::include_graphics("../man/figures/ESCO_ISCO_hierarchy.png")

## ----italian_occupations_example----------------------------------------------
library(labourR)
library(data.table)
library(magrittr)

corpus <- data.table(
  id = 1:3,
  text = c(
    "Insegnante di scuola primaria",
    "Sales and marketing assistant manager",
    "Data Scientist"
  )
)

## ----italian_language_identification------------------------------------------
corpus[, language := identify_language(text)]

## ----isco3_examples_match-----------------------------------------------------
languages <- unique(corpus$language)
suggestions <- lapply(languages, function(lang) {
  classify_occupation(
    corpus = corpus[language == lang],
    lang = lang,
    isco_level = 3,
    num_leaves = 10
  )
}) %>% rbindlist

## ----isco3_examples_print, echo=FALSE-----------------------------------------
suggestions

