## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(covid19swiss)

## -----------------------------------------------------------------------------
swiss_df <- refresh_covid19swiss()

head(swiss_df)

