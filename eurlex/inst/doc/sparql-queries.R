## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eurlex)

## ----makequery, message = FALSE, warning=FALSE, error=FALSE-------------------
library(eurlex)
library(dplyr)

query_dir <- elx_make_query(resource_type = "directive")

## ----precompute, include=FALSE------------------------------------------------
dirs <- elx_make_query(resource_type = "directive", include_date = TRUE, include_force = TRUE) %>% 
  elx_run_query()

results <- dplyr::select(dirs, -force,-date)

## -----------------------------------------------------------------------------
query_dir %>% 
  cat()

elx_make_query(resource_type = "caselaw") %>% 
  cat()

elx_make_query(resource_type = "manual", manual_type = "SWD") %>% 
  cat()


## -----------------------------------------------------------------------------
# minimal query: elx_make_query(resource_type = "directive")
elx_make_query(resource_type = "directive", include_date = TRUE, include_force = TRUE) %>% 
  cat()

# minimal query: elx_make_query(resource_type = "recommendation")
elx_make_query(resource_type = "recommendation", include_date = TRUE, include_lbs = TRUE) %>% 
  cat()

## -----------------------------------------------------------------------------
# request documents from directory 18 ("Common Foreign and Security Policy")
# and sector 3 ("Legal acts")
elx_make_query(resource_type = "any",
               directory = "18",
               sector = 3) %>% 
  cat()

