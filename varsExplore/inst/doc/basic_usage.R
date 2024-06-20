## ---- include = FALSE, paged.print=TRUE---------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message=FALSE, warning=FALSE
)

## ----setup--------------------------------------------------------------------
library(varsExplore)
library(dplyr)

## -----------------------------------------------------------------------------
qog <- rio::import("http://www.qogdata.pol.gu.se/dataarchive/qog_std_cs_jan18.dta")

## ---- eval=FALSE--------------------------------------------------------------
#  data.frame(Description = sjlabelled::get_label(qog)) %>% DT::datatable()

## ----echo=FALSE---------------------------------------------------------------
data.frame(Description = purrr::map_chr(qog, ~attr(.x, "label"))) %>% DT::datatable()

## ---- eval = FALSE------------------------------------------------------------
#  vars_explore(qog)

## -----------------------------------------------------------------------------
vdem_summary <- qog %>% 
  select(starts_with("vdem_")) %>%
  vars_explore(viewer = FALSE, silent = FALSE) %>% 
  select(-Values, -`Value labels`)

knitr::kable(vdem_summary)

