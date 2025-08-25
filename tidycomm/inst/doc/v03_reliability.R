## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include = FALSE---------------------------------------------------
library(tidycomm)

## -----------------------------------------------------------------------------
WoJ

## -----------------------------------------------------------------------------
WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  # Select variables of relevance for output
  dplyr::select(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)

## -----------------------------------------------------------------------------
WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4, type = "sum") %>%
  # Select variables of relevance for output
  dplyr::select(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4)

## -----------------------------------------------------------------------------
# Add two indices to data
WoJ <- WoJ %>% 
  add_index(ethical_flexibility, ethics_1, ethics_2, ethics_3, ethics_4) %>%
  add_index(trust_in_politics, trust_parliament, trust_government, trust_parties, trust_politicians)

WoJ %>% 
  get_reliability()

## -----------------------------------------------------------------------------
WoJ %>% 
  get_reliability(trust_in_politics)

## -----------------------------------------------------------------------------
WoJ %>% 
  get_reliability(type = 'omega', interval.type = 'mlr')

