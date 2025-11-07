## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----use_thai_preamble--------------------------------------------------------
.old_wd <- setwd(tempdir()) # for example only

thaipdf::use_thai_preamble()

setwd(.old_wd) # for example only

