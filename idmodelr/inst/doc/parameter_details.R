## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE
)

## ----setup, include = FALSE---------------------------------------------------
library(idmodelr)

## -----------------------------------------------------------------------------
if(!knitr:::is_latex_output()) {
 DT::datatable(parameter_details, filter = 'top', options = list(
  pageLength = nrow(parameter_details), autoWidth = TRUE
))
}else{
  knitr::kable(parameter_details)
  }

