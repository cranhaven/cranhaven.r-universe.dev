## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(tidycomm)

## -----------------------------------------------------------------------------
WoJ %>% 
  reverse_scale(autonomy_emphasis,
                lower_end = 1,
                upper_end = 5) %>% 
  dplyr::select(autonomy_emphasis,
                autonomy_emphasis_rev)

## -----------------------------------------------------------------------------
WoJ %>% 
  reverse_scale(autonomy_emphasis,
                name = "new_emphasis",
                lower_end = 1,
                upper_end = 5) %>% 
  dplyr::select(autonomy_emphasis,
                new_emphasis)

## -----------------------------------------------------------------------------
WoJ %>% 
  minmax_scale(autonomy_emphasis,
               change_to_min = 1,
               change_to_max = 10) %>% 
  dplyr::select(autonomy_emphasis,
                autonomy_emphasis_1to10)

## -----------------------------------------------------------------------------
WoJ %>% 
  center_scale(autonomy_selection) %>% 
  dplyr::select(autonomy_selection,
                autonomy_selection_centered)

## -----------------------------------------------------------------------------
WoJ %>% 
  z_scale(autonomy_selection) %>% 
  tab_frequencies(autonomy_selection,
                  autonomy_selection_z) %>% 
  visualize()

## -----------------------------------------------------------------------------
WoJ %>% 
  setna_scale(autonomy_emphasis, value = 5) %>% 
  dplyr::select(autonomy_emphasis, autonomy_emphasis_na)

## -----------------------------------------------------------------------------
WoJ %>% 
  dplyr::select(country) %>%
  recode_cat_scale(country, assign = c("Germany" = "german", "Switzerland" = "swiss"), other = "other")

## -----------------------------------------------------------------------------
WoJ %>%
  dplyr::select(autonomy_emphasis) %>%
  categorize_scale(autonomy_emphasis, 
               lower_end =1, upper_end =5,
               breaks = c(2, 3),
               labels = c("Low", "Medium", "High"))

## -----------------------------------------------------------------------------
WoJ %>% 
  dplyr::select(temp_contract) %>%
  dummify_scale(temp_contract)

