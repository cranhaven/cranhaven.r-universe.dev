## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, message = FALSE, warning = FALSE, include = FALSE-----------------
library(tidycomm)

## -----------------------------------------------------------------------------
WoJ

## -----------------------------------------------------------------------------
WoJ %>%  
  describe(autonomy_selection, autonomy_emphasis, work_experience)

## -----------------------------------------------------------------------------
WoJ %>% 
  describe()

## -----------------------------------------------------------------------------
WoJ %>%  
  dplyr::group_by(country) %>% 
  describe(autonomy_emphasis, autonomy_selection)

## -----------------------------------------------------------------------------
WoJ %>% 
  describe() %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  tab_percentiles()

## -----------------------------------------------------------------------------
WoJ %>% 
  tab_percentiles(trust_parties) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  describe_cat(reach, employment, temp_contract)

## -----------------------------------------------------------------------------
WoJ %>% 
  describe_cat()

## -----------------------------------------------------------------------------
WoJ %>% 
  dplyr::group_by(reach) %>% 
  describe_cat(country, employment)

## -----------------------------------------------------------------------------
WoJ %>% 
  describe_cat() %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>%  
  tab_frequencies(employment)

## -----------------------------------------------------------------------------
WoJ %>%  
  tab_frequencies(employment, country)

## -----------------------------------------------------------------------------
WoJ %>% 
  dplyr::group_by(country) %>%  
  tab_frequencies(employment)

## -----------------------------------------------------------------------------
WoJ %>% 
  tab_frequencies(country) %>% 
  visualize()

