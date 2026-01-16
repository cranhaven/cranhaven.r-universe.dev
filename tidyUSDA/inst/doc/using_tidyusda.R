## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyUSDA)

## -----------------------------------------------------------------------------
# Use keyring to store your api key
# key <- keyring::key_get("tidyusda")

# Or hard code that thing
key <- '7CE0AFAD-EF7B-3761-8B8C-6AF474D6EF71'  # please get your own key!

## -----------------------------------------------------------------------------
tidyUSDA::allCategory %>% head()

## -----------------------------------------------------------------------------
tidyUSDA::allGeogLevel %>% head()

## ----results = "hide", eval=FALSE---------------------------------------------
#  
#  # Get count of operations with sales in 2017
#  ops.with.sales <- tidyUSDA::getQuickstat(
#    sector=NULL,
#    group=NULL,
#    commodity=NULL,
#    category=NULL,
#    domain='TOTAL',
#    county=NULL,
#    key = key,
#    program = 'CENSUS',
#    data_item = 'CROP TOTALS - OPERATIONS WITH SALES',
#    geographic_level = 'STATE',
#    year = '2017',
#    state = NULL,
#    geometry = TRUE,
#    lower48 = TRUE,
#    weighted_by_area = T)

## ----eval=FALSE---------------------------------------------------------------
#  
#  # Plot this data for each state
#  tidyUSDA::plotUSDA(df = ops.with.sales)
#  

## ----eval=FALSE---------------------------------------------------------------
#  tidyUSDA::plotUSDA(df = ops.with.sales, fill_by = 'value_per_sq_mile')

