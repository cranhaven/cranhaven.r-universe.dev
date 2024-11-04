## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(idmodelr)

## -----------------------------------------------------------------------------
if(!knitr:::is_latex_output()) {
 DT::datatable(model_details, filter = 'top', options = list(
  pageLength = nrow(model_details), autoWidth = TRUE
))
}else{
  knitr::kable(model_details)
  }

